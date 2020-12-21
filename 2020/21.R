x <- readLines("21_data.txt")
#x <- readLines("21_sample.txt")

library(data.table)
d <- rbindlist(
  lapply(strsplit(x, " (contains ", fixed = TRUE), function(i){
    a <- unlist(strsplit(gsub(")", "", i[ 2 ], fixed = TRUE), ", ", fixed = TRUE))
    b <- unlist(strsplit(i[ 1 ], " "))
    cbind.data.frame(a = rep(a, each = length(b)),
                     b = b)
  }), idcol = "id")

# part 1 ------------------------------------------------------------------
res <- list()
i = 1
words <- unique(d$a)
aa = words[ i ]
while(TRUE){
  print(i); flush.console()
  intrs <- Reduce(intersect, split(d[a == aa & !(b %in% res), b], 
                                   d[a == aa & !(b %in% res), id]))
  if(length(intrs) == 1) res[ aa ] <- intrs 
  
  i <- i + 1
  if(i > length(words)){ i <- 1 }
  
  aa <- words[ i ]
  if(all(unique(d$a) %in% names(res))) break
}
sum(table(d[ !b %in% res, unique(b), by = id][, 2]))
# [1] 2262

# part 2 ------------------------------------------------------------------
res <- stack(res)
paste(res[ order(as.character(res$ind)), "values"], collapse = ",")
# [1] "txdmlzd,mptbpz,cxsvdm,vlblq,xbnmzr,rsbxb,glf,mtnh"
