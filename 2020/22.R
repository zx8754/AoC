x <- readLines("22_data.txt")
#x <- readLines("22_sample.txt")
x <- as.integer(x)
x <- x[ !is.na(x) ]

# part 1 ------------------------------------------------------------------
p1 <- head(x, length(x)/2)
p2 <- tail(x, length(x)/2)
r = 1
while(TRUE){
  print(r); flush.console()
  if(p1[ 1 ] > p2[ 1 ]){
    p1 <- c(p1[ 2:length(p1) ], p1[ 1 ], p2[ 1 ])
    if(length(p2) > 1){
      p2 <- p2[ 2:length(p2) ] } else { break }
  } else {
    p2 <- c(p2[ 2:length(p2) ], p2[ 1 ], p1[ 1 ])
    if(length(p1) > 1){
      p1 <- p1[ 2:length(p1) ]} else { break }
  }
  r <- r + 1
}
if(length(p1) > length(p2)){ res <- p1 } else { res <- p2 }
sum(res * length(res):1)
# [1] 35818

# part 2 ------------------------------------------------------------------
p1 <- head(x, length(x)/2)
p2 <- tail(x, length(x)/2)
r = 1
org1 <- p1
org2 <- p2
p01 <- p1[ 1 ]
p02 <- p2[ 1 ]

repeat{
  print(r); flush.console()
  if(p1[ 1 ] > p2[ 1 ]){
    p1 <- c(p1[ 2:length(p1) ], p1[ 1 ], p2[ 1 ])
    if(length(p2) > 1){
      p2 <- p2[ 2:length(p2) ] } else { p2 <- integer() }
  } else {
    p2 <- c(p2[ 2:length(p2) ], p2[ 1 ], p1[ 1 ])
    if(length(p1) > 1){
      p1 <- p1[ 2:length(p1) ]} else { p1 <- integer() }
  }
  r <- r + 1  
}

  

if(length(p1) > length(p2)){ res <- p1 } else { res <- p2 }
sum(res * length(res):1)
# [1] 35818


do