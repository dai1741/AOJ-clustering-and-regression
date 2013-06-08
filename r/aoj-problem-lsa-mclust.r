library(irlba)
library(mclust)
library(textir)

source("read-aoj-problem-data.r")
weighted.mat <- tfidf(simple_triplet_matrix(rows, cols, vals))

# reduce dimensions with randomized SVD
dim <- 100
mat2 <- spMatrix(max(weighted.mat$i), max(weighted.mat$j), weighted.mat$i, weighted.mat$j, weighted.mat$v)
compressed.mat <- irlba(mat2, dim, dim)
compressed.feats <- (compressed.mat$u %*% diag(compressed.mat$d) %*% t(compressed.mat$v[1:dim,]))

mclust.res <- Mclust(compressed.feats, G=50, modelNames="EEE")
# models other than "EEE" will fail when large number of clusters (30~) provided
# when used BIC model selection, I almost always got a result which says 'the best number of clusters is around 3~8.' But those clusters are too large to offer an insight, aren't they?
mclust.res$classification

clusters <- lapply( split(1:length(probs), mclust.res$classification),
  function(cluster) { unlist(lapply(cluster,
    function(n) { probs[[n]][["problemId"]] }))
  })
names(clusters) <- NULL  # remove names to pack as array
msgpack.writeResult("clusters-mixed-eee50.msg", pack(clusters))
