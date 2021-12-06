d <- readLines("05_data.txt")

m <- do.call(rbind, lapply(strsplit(d, split = " -> "),
                           function(i) unlist(strsplit(i, split = ","))))
m <- matrix(as.integer(m), ncol = 4)
#one based
m <- m + 1

#part 1 -----------------------------------------------------------------------
mSize <- max(m)
out <- matrix(0, nrow = mSize, ncol = mSize)

for(i in 1:nrow(m)){
  #i=1
  r <- m[ i, ]
  if(r[1] == r[3]){
    ix <- cbind(r[ 2 ]:r[ 4 ], r[ 1 ]) 
    out[ ix ] <- out[ ix ] + 1
  } else if(r[2] == r[4]) {
    ix <- cbind(r[ 2 ], r[ 1 ]:r[ 3 ]) 
    out[ ix ] <- out[ ix ] + 1
  }
}
sum(out > 1)
# [1] 6666


#part 2 -----------------------------------------------------------------------
out <- matrix(0, nrow = mSize, ncol = mSize)

for(i in 1:nrow(m)){
  r <- m[ i, ]
  if(r[1] == r[3] & r[2] != r[4]){
    ix <- cbind(r[ 2 ]:r[ 4 ], r[ 1 ]) 
    } else if(r[2] == r[4] & r[1] != r[3]) {
    ix <- cbind(r[ 2 ], r[ 1 ]:r[ 3 ]) 
    } else {
    ix <- cbind(seq(r[2], r[4]), seq(r[1], r[3]))
    }
  out[ ix ] <- out[ ix ] + 1
  }
sum(out > 1)
# [1] 19081

