m <- as.matrix(read.fwf("11_data.txt", widths = rep(1, 90)))
#m <- as.matrix(read.fwf("11_sample.txt", widths = rep(1, 10)))

# part 1 ------------------------------------------------------------------
cntHash1 <- function(mm, rr, cc){
  if(mm[ rr, cc ] == "."){ -1 } else {
    mB <- rbind(NA, cbind(NA, mm, NA), NA)
    cc = cc + 1
    rr = rr + 1
    m1 <- mB[ (rr - 1):(rr + 1), (cc - 1):(cc + 1)]
    m1[2, 2] <- "C" # Centre
    sum(m1 == "#", na.rm = TRUE)}
}

res1 <- res2 <- m
test = TRUE
cnt = 0
rc <- which(m != ".", arr.ind = TRUE)
while(test){
  cnt <- cnt + 1
  print(paste(cnt, sum(res1 == "#"))); flush.console()
  ix <- matrix(NA, nrow = nrow(m), ncol = ncol(m))
  ix[ rc ] <- apply(rc, 1, function(i) cntHash1(res2, i[ 1 ], i[ 2 ]))
  res1[ res2 == "L" & ix == 0 ] <- "#"
  res1[ res2 == "#" & ix >= 4 ] <- "L"
  if(identical(res1, res2)){ test = FALSE } else {res2 <- res1}
}

sum(c(res1 == "#"))
#[1] 2093

# part 2 ------------------------------------------------------------------
notDot <- function(x) x[ x != "." ][ 1 ]

v = col(m)
h = row(m)
d1 <- v - h
d2 <- v + h

cntHash2 <- function(mm, rr, cc){
  sum(sapply(
    list(
      N  = rev(mm[ h < rr & v == cc  ]),
      E  = mm[ h == rr & v > cc ],
      S  = mm[ h > rr & v == cc ],
      W  = rev(mm[ h  == rr & v < cc ]),
      NE = mm[ h < rr & d2 == d2[ rr, cc ] ],
      SE = mm[ h > rr & d1 == d1[ rr, cc ] ],
      SW = rev(mm[ h > rr & d2 == d2[ rr, cc ] ]),
      NW = rev(mm[ h < rr & d1 == d1[ rr, cc ] ])
    ), function(x) x[ x != "." ][ 1 ] == "#"), na.rm = TRUE)
}

res1 <- res2 <- m
test = TRUE
cnt = 0
rc <- which(m != ".", arr.ind = TRUE)
while(test){
  cnt <- cnt + 1
  print(paste(cnt, sum(res1 == "#"))); flush.console()
  ix <- matrix(NA, nrow = nrow(m), ncol = ncol(m))
  ix[ rc ] <- apply(rc, 1, function(i) cntHash2(res2, i[ 1 ], i[ 2 ]))
  res1[ res2 == "L" & ix == 0 ] <- "#"
  res1[ res2 == "#" & ix >= 5 ] <- "L"
  if(identical(res1, res2)){ test = FALSE } else {res2 <- res1}
}
