library(igraph)
n = 100
d <- as.matrix(read.fwf("15_data.txt", rep(1, n)))

#udf --------------------------------------------------------------------------
makegdf <- function(x){
  nn <- ncol(x)
  rc <- matrix(paste0(row(x), "_", col(x)), nrow = nn)
  rbind(
    #right
    data.frame(from = as.vector(rc[, 1:(nn - 1) ]), to = as.vector(rc[, 2:nn ]),
               weight = as.vector(x[, 1:(nn - 1)])),
    #down
    data.frame(from = as.vector(rc[1:(nn - 1), ]), to = as.vector(rc[2:nn, ]),
               weight = as.vector(x[1:(nn - 1), ])),
    #left
    data.frame(from = as.vector(rc[, 2:nn ]), to = as.vector(rc[, 1:(nn - 1) ]),
               weight = as.vector(x[, 2:nn])),
    #up
    data.frame(from = as.vector(rc[2:nn, ]), to = as.vector(rc[1:(nn - 1), ]),
               weight = as.vector(x[2:nn, ]))
  )}

#part 1 -----------------------------------------------------------------------
g <- graph_from_data_frame(makegdf(d), directed = TRUE)
distances(g, v = "1_1", to = paste0(n, "_", n), 
          mode = "in", algorithm = "dijkstra")
# [1] 626

#part 2 -----------------------------------------------------------------------
# 5x bigger
nn = n * 5
out <- matrix(0, nrow = nn, ncol = nn)
rc <- seq(1, nn, n)
out[ 1:n, 1:n ] <- d
for(i in 2:5){
  x <- out[ rc[ i - 1 ]:(rc[ i - 1 ] + n - 1), rc[ 1 ]:n ] + 1
  x[ x == 10 ] <- 1
  out[ rc[ i ]:(rc[ i ] + n - 1), rc[ 1 ]:(rc[ 2 ] - 1) ] <- x
}
for(i in 2:5){
  x <- out[ 1:(n * 5), rc[ i - 1 ]:(rc[ i ] - 1) ] + 1
  x[ x == 10 ] <- 1
  out[ 1:(n * 5), rc[ i ]:(rc[ i ] + n - 1) ] <- x
}

g <- graph_from_data_frame(makegdf(out), directed = TRUE)
distances(g, v = "1_1", to = paste0(nn, "_", nn), 
          mode = "in", algorithm = "dijkstra")
# 2966
