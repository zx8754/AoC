d <- readLines("09_data.txt")
m <- t(sapply(d, function(i) as.integer(strsplit(as.character(i), split = "")[[1]])))

#part 1 -----------------------------------------------------------------------
x <- Inf
mm <- rbind(x, cbind(x, m, x), x)

ix <- which(!is.infinite(mm), arr.ind = TRUE)
uu <- cbind(ix[, 1] - 1, ix[, 2])
rr <- cbind(ix[, 1] , ix[, 2] + 1)
dd <- cbind(ix[, 1] + 1, ix[, 2])
ll <- cbind(ix[, 1] , ix[, 2] - 1)

resix <- mm[ ix ] < mm[ uu ] & mm[ ix ] < mm[ rr ] &
  mm[ ix ] < mm[ dd ] & mm[ ix ] < mm[ ll ]

sum(mm[ ix ][ resix ] + 1)
# [1] 456

#part 2 -----------------------------------------------------------------------
library(igraph)

x <- 9
mm <- rbind(x, cbind(x, m, x), x)
mm[ !mm == 9 ] <- 1
mm[ mm == 9 ] <- 9

id <- as.vector(mm)
g <- graph.lattice(dim(mm))
el <- get.edgelist(g)
g <- delete.edges(g, E(g)[ id[el[, 1]] != id[el[, 2]] ])

# drop 9, then top 3
prod(sort(clusters(g)$csize, decreasing = TRUE)[ 2:4 ])
# [1] 1047744
