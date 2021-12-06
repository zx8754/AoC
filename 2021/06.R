d <- as.integer(strsplit(readLines("06_data.txt"), ",")[[1]])

#part 1 -----------------------------------------------------------------------
res <- d
for(i in 1:80){
  #i=2
  newFish = vector("integer")
  if(any(res == 0)) newFish <- rep(8, sum(res == 0))
  res <- res - 1
  res[ res < 0 ] <- 6
  res <- c(res, newFish)
  
}
length(res)
# [1] 343441

#part 2 -----------------------------------------------------------------------
res <- as.numeric(table(factor(d, levels = 0:8)))

for(i in 1:256){
  pos0 <- res[ 1 ]
  res <- c(res[ 2:9 ], res[ 1 ])
  res[ 7 ] <- res[ 7 ] + pos0
  }

format(sum(res), scientific = FALSE)
# [1] "1569108373832"
