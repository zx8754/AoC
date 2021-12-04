d <- as.matrix(read.fwf("03_data.txt", widths = rep(1, nchar(readLines("03_data.txt")[1]))))

#part 1 -----------------------------------------------------------------------
toInt <- function(x) strtoi(paste(x, collapse = ""), base = 2)
res1 <- as.integer(colSums(d) > nrow(d)/2)
res2 <- as.integer(res1 == 0)
toInt(res1) * toInt(res2)
#[1] 3687446

#part 2 -----------------------------------------------------------------------
res1 = d
ix = 1
while (nrow(res1) > 1) {
  n = sum(res1[, ix] == 1)
  if(n >= nrow(res1) - n){
    res1 <- res1[ res1[, ix ] == 1, , drop = FALSE]
  } else {
    res1 <- res1[ res1[, ix ] == 0, , drop = FALSE]
  }
  ix <- ix + 1
}

res2 = d
ix = 1
while (nrow(res2) > 1) {
  n = sum(res2[, ix] == 0)
  if(n <= (nrow(res2) - n)){
    res2 <- res2[ res2[, ix ] == 0, , drop = FALSE]
  } else {
    res2 <- res2[ res2[, ix ] == 1, , drop = FALSE]
  }
  ix <- ix + 1
}

toInt(res1) * toInt(res2)
# [1] 4406844
