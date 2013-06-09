library(msgpackR)
library(lsa)

# source("read-aoj-problem-data.r")

writeClustersFromClasses <- function(classes, file) {
  clusters <- lapply( split(1:length(probs), classes),
    function(cluster) { unlist(lapply(cluster, function(n) { prob.ids[n] })) })
  names(clusters) <- NULL  # remove names to pack as array
  msgpack.writeResult(file, msgpackR::pack(clusters))
}

doSkmeans <- function(file="clusters-50.msg", k=50, popsize=6, maxiter=50) {
  library(skmeans)

  set.seed(1)  # be reproducible
  result <- skmeans(weighted.feats, k=k, control=list(popsize=popsize, maxiter=maxiter))  # do clustering
  writeClustersFromClasses(file, result$cluster)

  result
}
# doSkmeans("clusters-70.msg", k=70, popsize=12, maxiter=60)

doMclust <- function(file="clusters-mixed.msg", G=1:9, modelNames=NULL) {
  library(mclust)
  
  set.seed(1)  # mclustランダム要素あったっけ？
  mclust.fit <- Mclust(compressed.feats, G=G, modelNames=modelNames)
  writeClustersFromClasses(file, mclust.fit$classification)

  mclust.fit
}
# doMclust("clusters-mixed-eee50.msg", G=50, modelNames="EEE")

doHclust <- function(file="clusters-h.msg", method="euclid") {
  dists <- switch(method,
    spherical= as.dist(acos(cosine(as.matrix(t(weighted.feats))))),  # spherical distance on unit hypersphere
    cosine= 1 - as.dist(cosine(as.matrix(t(weighted.feats)))),  # cosine disimilarity
    dist(compressed.feats, method=method)  # euclid or manhattan or something
  )
  hc <- hclust(dists, method="ward")
  tree <- dendrapply(as.dendrogram(hc),
    function(n) {
      if (is.leaf(n)) prob.ids[attr(n, "label")]
    } )
  msgpack.writeResult(file, msgpackR::pack(tree))

  hc
}
# doHclust("clusters-h-spherical.msg", method="spherical")


getDifficulty <- function(easy.thres=10000, easy.score=60) {
  # read data frame. col1: problem id, col2: difficulty score
  # data from: https://docs.google.com/spreadsheet/ccc?key=0Ank515IguQc4dGJDR3FYOGZGTXQ5VHhNa1JmMDB4U0E#gid=0
  scores <- read.table('aoj-difficluty-list.txt')
  solved.inds <- which(scores[, 1] %in% prob.ids)
  unsolved.prob.ids <- scores[-solved.inds, ]
  scores <- scores[solved.inds, ]  # take away problems not listed in feature data
  ind.in.mat <- match(scores[, 1], prob.ids)  # search index in the given data

  # 難易度表だけじゃ回帰無理でした、Volume0の問題が難易度500くらいになってしまいます
  # ID0000～0020とID1000の問題について自前で難易度を設定しました（全て20以上90以下）
  # あと一定数以上に解かれてて難易度未定義な問題は難易度60ってことでお願いします
  easy.inds <- which(table(rows) >= easy.thres & !(1:length(probs) %in% ind.in.mat))
  ind.in.mat <- c(ind.in.mat, easy.inds)
  
  list(ind.in.mat = ind.in.mat,
       values = c(scores[, 2], rep(easy.score, length(easy.inds))),
       compressed.feats = compressed.feats[ind.in.mat, ],
       weighted.feats = weighted.feats[ind.in.mat, ],
       unsolved.prob.ids = unsolved.prob.ids)
}

doRegressionGlmnet <- function(file="scores-lasso.msg", alpha=1, s=0.01) {
  library(glmnet)

  scores <- getDifficulty(600, 60)

  set.seed(1)  # おそらく不要
  # 少なくともelastic-netとlassoは高次元と変数の相関に強くないので次元削減したのを使う
  lnet.fit <- glmnet(as.matrix(scores$compressed.feats), scores$values, alpha=alpha)
  # predict(lnet.fit, newx=as.matrix(compressed.feats[match(201:300, prob.ids), ]), s=0.1)
  preds <- predict(lnet.fit, newx=as.matrix(compressed.feats), s=s)
  data.frame(prob.ids, preds, 1:length(probs) %in% scores$ind.in.mat)

  if (!is.null(file)) msgpack.writeResult(file, msgpackR::pack(list(problemIds=prob.ids, scores=as.numeric(preds))))

  list(lnet=lnet.fit, pred=preds)
}
# cv.glmnet(as.matrix(scored.feats), scores$values, alpha=0.5)
# doRegressionGlmnet(file="scores-ridge.msg", alpha=0, s=21)  # これはweighted.featsを使う
# doRegressionGlmnet(file="scores-lasso.msg", alpha=1, s=0.01)
# lnets <- doRegressionGlmnet(file="scores-elastic.msg", alpha=0.5, s=0.35)

doRegressionRf <- function(file="scores-rf.msg", ntree=1000) {
  library(randomForest)

  scores <- getDifficulty(600, 60)

  set.seed(1)
  rf <- randomForest(scores$weighted.feats, scores$values, ntree=ntree)
  plot(rf)
  pred <- predict(rf, as.matrix(weighted.feats))
  print( data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat) )

  if (!is.null(file)) msgpack.writeResult(file, msgpackR::pack(list(problemIds=prob.ids, scores=as.numeric(pred))))

  list(rf=rf, pred=pred)
}

doRegressionNnet <- function(file="scores-nnet.msg") {
  library(nnet)

  scores <- getDifficulty(600, 60)

  set.seed(1)
  nnet.fit <- nnet(as.matrix(scores$weighted.feats), scores$values / max(scores$values),
    size=3, MaxNWts=20000, maxit=1000, decay=0.001)
  pred <- predict(nnet.fit, weighted.feats) * max(scores$values)
  print( data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat) )

  if (!is.null(file)) msgpack.writeResult(file, msgpackR::pack(list(problemIds=prob.ids, scores=as.numeric(pred))))

  list(nnet=nnet.fit, pred=pred)
}

doRegressionEpsSvr <- function(file="scores-epssvr.msg", kernel="vanilladot", C=2, epsilon=0.08) {
  library(kernlab)

  scores <- getDifficulty(600, 60)

  epssvm <- ksvm(scores$compressed.feats, rscores$values / max(scores$values), type="eps-svr", kernel=kernel, C=C, epsilon=epsilon)
  pred <- predict(epssvm, compressed.feats) * max(scores$values)
  print( data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat) )

  if (!is.null(file)) msgpack.writeResult(file, msgpackR::pack(list(problemIds=prob.ids, scores=as.numeric(pred))))

  list(svr=epssvm, pred=pred)
}
# doRegressionEpsSvr(file="scores-epssvr-vanilladot.msg", kernel="vanilladot", C=2, epsilon=0.08)
# doRegressionEpsSvr(file="scores-epssvr-rbfdot.msg", kernel="rbfdot", C=8, epsilon=0.05)

genTrainingScoreMsg <- function(file="training-scores.msg") {
  scores <- getDifficulty(600, 60)
  msgpack.writeResult(file, msgpackR::pack(list(problemIds=prob.ids[scores$ind.in.mat], scores=scores$values)))
}
# 今更だが別にmsgpackじゃなくてjsonでよかった
