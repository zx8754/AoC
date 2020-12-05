x <- readLines("05_data.txt")

# part1
rows <- 0:127
cols <- 0:7
seatIDs <- sapply(strsplit(x, ""), function(i){
  fb <- i[1:7]
  lr <- i[8:10]
  #find row
  rr <- matrix(rows, ncol = 2)
  for(j in fb) rr <- matrix(rr[, match(j, c("F", "B"))], ncol = 2)
  #find col
  cc <- matrix(cols, ncol = 2)
  for(j in lr) cc <- matrix(cc[, match(j, c("L", "R"))], ncol = 2)
  #seat id
  rr[ 1 ] * 8 + cc[ 1 ]
})
max(seatIDs)
# [1] 835

# part2
allSeats <- seq(min(seatIDs), max(seatIDs))
allSeats[ which(!allSeats %in% seatIDs) ]
# [1] 649
