numbers <- scan("04_data.txt", nlines = 1, sep = ",")
d2 <- read.table("04_data.txt", skip = 2)
n = nrow(d2)/5
d2 <- lapply(split(d2, gl(n, 5)), as.matrix)

#part 1 -----------------------------------------------------------------------
drawN = 4
check = FALSE
while(!check){
  drawN <- drawN + 1
  draw <- numbers[ 1:drawN ]
  for(whichBoard in seq(d2)){
    bb <- d2[[ whichBoard ]]
    res <- matrix(bb %in% draw, ncol = 5)
    whichRow = which(rowSums(res) == 5)
    whichCol = which(colSums(res) == 5)
    check <- length(c(whichRow, whichCol)) > 0
    if(check) break
  }
  }

board <- d2[[ whichBoard ]]
board[ res ] <- NA

sum(board, na.rm = TRUE) * tail(draw, 1)
# [1] 82440

#part 2 -----------------------------------------------------------------------
drawN = 4
check = rep(0, length(d2))
winOrder = 1
while(sum(check != 0) < length(d2)){
  drawN <- drawN + 1
  draw <- numbers[ 1:drawN ]
  for(whichBoard in which(check == 0)){
    bb <- d2[[ whichBoard ]]
    res <- matrix(bb %in% draw, ncol = 5)
    whichRow = which(rowSums(res) == 5)
    whichCol = which(colSums(res) == 5)
    if(length(c(whichRow, whichCol)) > 0) {
      check[ whichBoard ] <- winOrder
      winOrder <- winOrder + 1
      }
  }
}

board <- d2[[ which.max(check) ]]
ix <- matrix(board %in% draw, ncol = 5)
board[ ix ] <- NA
sum(board, na.rm = TRUE) * tail(draw, 1)
# [1] 20774

