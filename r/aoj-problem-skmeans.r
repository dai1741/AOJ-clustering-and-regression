library(textir)
library(skmeans)

# read data and convert to sparse matrix
source("read-aoj-problem-data.r")

set.seed(1)  # be reproducible
result <- skmeans(weighted.mat, k=70, control=list(popsize=12, maxiter=60))  # do clustering

# collect partitions
clusters <- lapply( split(1:length(probs), result$cluster),
  function(cluster) { unlist(lapply(cluster,
    function(n) { probs[[n]][["problemId"]] }))
  })
names(clusters) <- NULL  # remove names to pack as array
msgpack.writeResult("clusters-70.msg", pack(clusters))
