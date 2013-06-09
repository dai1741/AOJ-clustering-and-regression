library(textir)
library(irlba)
library(msgpackR)

# !!! ALL VARS DECLARED HERE ARE GLOBAL !!!

# read data and get triplet
probs <- msgpackR::unpack("../aoj-problem-data.msg")
rows <- unlist(mapply(function(p,n) { rep(n,length(p[["solvers"]])) }, probs, 1:length(probs)))
cols <- unlist(lapply(probs, function(p) { p[["solvers"]]+1 } ))  # 1-indexed in R
vals <- unlist(lapply(probs, function(p) { p[["codeSizes"]] } ))

prob.ids <- unlist(lapply(1:length(probs), function(n) { probs[[n]][["problemId"]] }))

feats <- simple_triplet_matrix(rows, cols, vals)
weighted.feats <- tfidf(feats)

# reduce dimensions with randomized SVD
reduceSimpleTripletMatrixDim <- function (mat, dim) {
  sp.mat <- spMatrix(max(mat$i), max(mat$j), mat$i, mat$j, mat$v)
  svd.res <- irlba(sp.mat, dim, dim)
  svd.res$u %*% diag(svd.res$d) %*% t(svd.res$v[1:dim,])
}

set.seed(1)
reduced.dim <- 100
compressed.feats <- reduceSimpleTripletMatrixDim(weighted.feats, reduced.dim)
