x <- as.integer(unlist(strsplit("368195742", "")))
# x <- as.integer(unlist(strsplit("389125467", "")))
current <- 1
three <- 2:4
res <- x
m = 1
while(TRUE){
  dest <- x[ current ] - 1
  if(dest %in% setdiff(res, x[ c(current, three)])){
    res <- res
    current <- dest
  }
  print(paste(m, "current", current, 
              "three", paste(three, collapse = ","),
              "dest", dest)); flush.console()
  if(m == 10) break
  m <- m + 1
}
