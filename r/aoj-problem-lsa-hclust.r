library(irlba)
library(hclust)
library(textir)
library(lsa)

source("read-aoj-problem-data.r")
weighted.mat <- tfidf(simple_triplet_matrix(rows, cols, vals))

# reduce dimensions with randomized SVD
dim <- 100
mat2 <- spMatrix(max(weighted.mat$i), max(weighted.mat$j), weighted.mat$i, weighted.mat$j, weighted.mat$v)
compressed.mat <- irlba(mat2, dim, dim)
compressed.feats <- (compressed.mat$u %*% diag(compressed.mat$d) %*% t(compressed.mat$v[1:dim,]))

# dists <- dist(compressed.feats)  # euqulid distance in reduced dim
# dists <- 1 - as.dist(cosine(as.matrix(t(weighted.mat))))  # cosine disimilarity
dists <- as.dist(acos(cosine(as.matrix(t(weighted.mat)))))  # spherical distance on unit hypersphere

hc <- hclust(dists, method="ward")
# subhc <- hclust(dist(compressed.feats[group[[4]], ]), method="ward")
# plot( subhc, label=lapply(1:length(probs), function(n) { probs[[n]][["problemId"]] })[group[[4]]] )

tree <- dendrapply(as.dendrogram(hc),
function(n) {
  if (is.leaf(n)) probs[[attr(n, "label")]][["problemId"]]
} )
msgpack.writeResult("clusters-h.msg", pack(tree))
