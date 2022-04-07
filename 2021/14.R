# data ------------------------------------------------------------------------
d <- readLines("14_data.txt")
p <- d[ 1 ]
dict <- d[ 3:length(d) ]
dict <- do.call(rbind, strsplit(dict, " -> ", fixed = TRUE))
dict <- setNames(dict[, 2], dict[, 1])

#part 1 -----------------------------------------------------------------------
res <- p
for(i in 1:10){
  n <- nchar(res)
  twos <- substring(res, 1:n, c(2:n, n))
  m <- vector("character", length(twos))
  for(t in seq_along(m)){
    x <- dict[ twos[ t ] ]
    if(is.na(x)) { m[ t ] <- twos[ t ] } else { m[ t ] <- paste0(substr(twos[ t ], 1, 1), x) }
  }
  res <- paste(m, collapse = "")
  }

out <- table(strsplit(res, "")[[ 1 ]])
max(out) - min(out)

#part 2 -----------------------------------------------------------------------
