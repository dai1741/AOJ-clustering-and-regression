library(msgpackR)
library(lsa)

source("read-aoj-problem-data.r")

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
  
  set.seed(1)  # mclustƒ‰ƒ“ƒ_ƒ€—v‘f‚ ‚Á‚½‚Á‚¯H
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
