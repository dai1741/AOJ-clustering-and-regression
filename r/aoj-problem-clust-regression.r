library(msgpackR)
library(lsa)

# source("read-aoj-problem-data.r")

writeMsgIfCan <- function(file=NULL, obj) {
  if (!is.null(file)) msgpack.writeResult(file, msgpackR::pack(obj))
}

writeClustersFromClasses <- function(file, classes) {
  clusters <- lapply( split(1:length(probs), classes),
    function(cluster) { unlist(lapply(cluster, function(n) { prob.ids[n] })) })
  names(clusters) <- NULL  # remove names to pack as array
  writeMsgIfCan(file, clusters)
}

doSkmeans <- function(file="clusters-50.msg", k=50, popsize=6, maxiter=50) {
  library(skmeans)

  set.seed(1)  # be reproducible
  result <- skmeans(weighted.feats, k=k, control=list(popsize=popsize, maxiter=maxiter))  # do clustering
  writeClustersFromClasses(file, result$cluster)

  result
}

doMclust <- function(file="clusters-mixed.msg", G=1:9, modelNames=NULL) {
  library(mclust)
  
  set.seed(1)  # mclustランダム要素あったっけ？
  mclust.fit <- Mclust(compressed.feats, G=G, modelNames=modelNames)
  writeClustersFromClasses(file, mclust.fit$classification)

  mclust.fit
}

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
  writeMsgIfCan(file, tree)

  hc
}


getDifficulty <- function(easy.thres=10000, easy.score=60) {
  # read data frame. col1: problem id, col2: difficulty score
  # data from: https://docs.google.com/spreadsheet/ccc?key=0Ank515IguQc4dGJDR3FYOGZGTXQ5VHhNa1JmMDB4U0E#gid=0
  scores <- read.table('aoj-difficluty-list.txt')
  solved.inds <- which(scores[, 1] %in% prob.ids)
  unsolved.prob.ids <- scores[-solved.inds, ]
  scores <- scores[solved.inds, ]  # take away problems not listed in feature data
  ind.in.mat <- match(scores[, 1], prob.ids)  # search index in the given data

  # 難易度表だけだとうまく回帰できなかったので、
  # 一定数以上に解かれていて難易度未定義な問題は適当に低く難易度設定する
  easy.inds <- which(table(rows) >= easy.thres & !(1:length(probs) %in% ind.in.mat))
  ind.in.mat <- c(ind.in.mat, easy.inds)
  
  list(ind.in.mat = ind.in.mat,
       values = c(scores[, 2], rep(easy.score, length(easy.inds))),
       compressed.feats = compressed.feats[ind.in.mat, ],
       weighted.feats = weighted.feats[ind.in.mat, ],
       unsolved.prob.ids = unsolved.prob.ids)
}

getDifficultyWithAutoEasy <- function() getDifficulty(easy.thres=600, easy.score=60)

doRegressionGlmnet <- function(file="scores-lasso.msg", alpha=1, s=0.01) {
  library(glmnet)

  scores <- getDifficultyWithAutoEasy()

  set.seed(1)  # おそらく不要
  # Ridge以外は高次元と変数の相関に強くないので次元削減したのを使う
  scored.feats <- if (alpha == 0) scores$weighted.feats else scores$compressed.feats
  new.feats <- if (alpha == 0) weighted.feats else compressed.feats
  lnet.fit <- glmnet(as.matrix(scored.feats), scores$values, alpha=alpha)
  pred <- predict(lnet.fit, newx=as.matrix(new.feats), s=s)
  data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat)

  writeMsgIfCan(file, list(problemIds=prob.ids, scores=as.numeric(pred)))

  list(lnet=lnet.fit, pred=pred)
}
# cv.glmnet(as.matrix(scored.feats), scores$values, alpha=0.5)  # 交差検定あんまり役に立たない…

doRegressionRf <- function(file="scores-rf.msg", ntree=1000, mtry=790) {
  library(randomForest)

  scores <- getDifficultyWithAutoEasy()

  set.seed(1)
  rf <- randomForest(as.matrix(scores$weighted.feats), scores$values, ntree=ntree)
  plot(rf)
  pred <- predict(rf, as.matrix(weighted.feats))
  print( data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat) )

  writeMsgIfCan(file, list(problemIds=prob.ids, scores=as.numeric(pred)))

  list(rf=rf, pred=pred)
}

doRegressionNnet <- function(file="scores-nnet.msg", size=3, maxit=100, decay=0) {
  library(nnet)

  scores <- getDifficultyWithAutoEasy()

  set.seed(1)
  nnet.fit <- nnet(as.matrix(scores$weighted.feats), scores$values / max(scores$values),
    size=size, MaxNWts=30000, maxit=maxit, decay=decay)
  pred <- predict(nnet.fit, weighted.feats) * max(scores$values)
  print( data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat) )

  writeMsgIfCan(file, list(problemIds=prob.ids, scores=as.numeric(pred)))

  list(nnet=nnet.fit, pred=pred)
}

doRegressionEpsSvr <- function(file="scores-epssvr.msg", kernel="vanilladot", C=1, epsilon=0.1) {
  library(kernlab)

  scores <- getDifficultyWithAutoEasy()

  epssvm <- ksvm(as.matrix(scores$weighted.feats), scores$values / max(scores$values),
    type="eps-svr", kernel=kernel, C=C, epsilon=epsilon)
  pred <- predict(epssvm, as.matrix(weighted.feats)) * max(scores$values)
  print( data.frame(prob.ids, pred, 1:length(probs) %in% scores$ind.in.mat) )

  writeMsgIfCan(file, list(problemIds=prob.ids, scores=as.numeric(pred)))

  list(svr=epssvm, pred=pred)
}


genTrainingScoreMsg <- function(file="training-scores.msg") {
  scores <- getDifficultyWithAutoEasy()
  writeMsgIfCan(file, list(problemIds=prob.ids[scores$ind.in.mat], scores=scores$values))
}
# 今更だが別にmsgpackじゃなくてjsonでよかった

# うまい具合にできたやつだけまとめて吐く
genAllMsgFiles <- function() {
  doSkmeans("clusters-sk-50.msg", k=50, popsize=12, maxiter=70)
  doSkmeans("clusters-sk-70.msg", k=70, popsize=12, maxiter=60)
  doMclust("clusters-mixed-eee50.msg", G=50, modelNames="EEE")
  doHclust("clusters-h-spherical.msg", method="spherical")

  doRegressionGlmnet(file="scores-ridge.msg", alpha=0, s=21)
  doRegressionGlmnet(file="scores-lasso.msg", alpha=1, s=0.01)
  doRegressionGlmnet(file="scores-elastic.msg", alpha=0.5, s=0.35)
  doRegressionRf(file="scores-rf.msg", ntree=1000)
  doRegressionNnet(file="scores-nnet.msg", size=4, maxit=1000, decay=0.001)
  doRegressionEpsSvr(file="scores-epssvr-vanilladot.msg", kernel="vanilladot", C=1, epsilon=0.04)
  doRegressionEpsSvr(file="scores-epssvr-rbfdot.msg", kernel="rbfdot", C=3, epsilon=0.03)

  genTrainingScoreMsg()
}
