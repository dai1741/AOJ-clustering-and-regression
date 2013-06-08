library(msgpackR)
library(textir)

# read data and get triplet
probs <- unpack("../aoj-problem-data.msg")
rows <- unlist(mapply(function(p,n) { rep(n,length(p[["solvers"]])) }, probs, 1:length(probs)))
cols <- unlist(lapply(probs, function(p) { p[["solvers"]]+1 } ))  # 1-indexed in R
vals <- unlist(lapply(probs, function(p) { p[["codeSizes"]] } ))

mat <- simple_triplet_matrix(rows, cols, vals)
weighted.mat <- tfidf(mat)
