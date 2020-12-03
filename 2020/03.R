x <- readLines("03_data.txt")

# part 1
m <- t(sapply(strsplit(x, ""), rep, times = 3))
ix <- cbind(seq(1, nrow(m)), seq(1, ncol(m), 3))
table(m[ ix ])
#   #   . 
# 284  39

# part 2
foo <- function(i){
  m <- t(sapply(strsplit(x, ""), rep, times = i[ 1 ]))
  ix <- cbind(seq(1, nrow(m), i[ 2 ]), seq(1, ncol(m), i[ 1 ]))
  unname(table(m[ ix ])[ "#" ])
}

prod(sapply(list(c(1,1), c(3, 1), c(5, 1), c(7, 1), c(1, 2)), foo))
# [1] 3510149120
