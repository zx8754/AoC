x <- read.table("08_data.txt")

# part 1 ------------------------------------------------------------------
foo <- function(x){
  test = TRUE
  s <- ix <- 1
  t <- 0
  while(test & ix <= nrow(x)){
    c1 <- x[ ix, 1 ]
    c2 <- x[ ix, 2 ]
    if(c1 == "nop"){ 
      ix <- ix + 1
    } else if(c1 == "acc"){ 
      t <- t + c2
      ix <- ix + 1
    } else if(c1 == "jmp" & c2 > 0){
      ix <- ix + c2 
    } else if(c1 == "jmp" & c2 == 0){
      ix <- ix + 1 
    } else if(c1 == "jmp" & c2 < 0 & (ix + c2) %in% s){
      test <- FALSE
    } else {
      ix <- ix + c2
    }
    s <- c(s, ix)
    }
  #return
  c(totalAcc = t, lastRow = ix)
}
foo(x)
# totalAcc  lastRow 
#     1723      258 

# part 2 ------------------------------------------------------------------
res = 0
for(i in which(x$V1 %in% c("nop", "jmp"))){
  d <- x
  d[ i, "V1"] <- ifelse(x[ i, "V1"] == "nop", "jmp", "nop")
  f <- foo(d)
  if(f[ "lastRow" ] == (nrow(x)+1)){
    res <- f[ "totalAcc" ]
    break}
}
res
# totalAcc 
#      846 