x <- c(1,0,18,10,19,6)
#x <- c(0,3,6)

# part 1 ------------------------------------------------------------------
res <- rep(NA_integer_, 2020)
res[ 1:c(length(x)) ] <- x
lastSpoken <- tail(x, 1)

for(t in (length(x) + 1):2020){
  #t=4
  s <- which(res[ 1:t ] == lastSpoken)
  if(length(s) == 0){
    res[ t ] <- lastSpoken
  } else if(length(s) == 1){
    res[ t ] <- lastSpoken <- 0
  } else {
    lastSpoken <- diff(tail(s, 2))
    res[ t ] <- lastSpoken  
  }
}
res[ 2020 ]
# [1] 441

# part 2 v2 hash ----------------------------------------------------------
# takes around 20-30 mins
x <- c(1,0,18,10,19,6)
#x <- c(0,3,6)

H <- new.env()
for(i in seq.int(x)){
  H[[ paste0("n", x[ i ]) ]] <- c(i, i)
}

t1 <- Sys.time()
names(H)
lastSpoken <- paste0("n", tail(x, 1))
#n = 2020
n = 30000000

for(t in (length(x) + 1):n){
  if(is.null(H[[ lastSpoken ]])){
    lastSpoken <- "n0"
    H[[ lastSpoken ]] <- c(H[[ lastSpoken ]][ 2 ], t)
  } else {
    lastSpoken <- paste0("n", diff(tail(H[[ lastSpoken ]], 2)))
    if(is.null(H[[ lastSpoken ]])){
      H[[ lastSpoken ]] <- c(t, t)
    } else {
      H[[ lastSpoken ]] <- c(H[[ lastSpoken ]][ 2 ], t)  
    }
  }
}
lastSpoken
# [1] "n10613991"

# part 2 v1 ---------------------------------------------------------------
#v1 fail, will take 4-5 days...
# x <- c(1,0,18,10,19,6)
# res <- rep(NA_integer_, 30000000)
# res[ 1:c(length(x)) ] <- x
# lastSpoken <- tail(x, 1)
# 
# for(t in (length(x) + 1):30000000){
#   #t=4
#   s <- which(res[1:t] == lastSpoken)
#   if(length(s) == 0){
#     res[ t ] <- lastSpoken
#   } else if(length(s) == 1){
#     res[ t ] <- lastSpoken <- 0
#   } else {
#     lastSpoken <- diff(tail(s, 2))
#     res[ t ] <- lastSpoken  
#   }
#   print(t); flush.console()
# }
# res[ 30000000 ]
