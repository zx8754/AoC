d <- read.fwf("11_data.txt", widths = rep(1, 10))

#part 1 -----------------------------------------------------------------------
around <- function(i){
  rn = i[1]; cn = i[2]
  rbind(c(rn - 1, cn),
        c(rn + 1, cn),
        cbind(c(rn - 2) + 1:3, cn - 1),
        cbind(c(rn - 2) + 1:3, cn + 1)) }

m <- rbind(NA, cbind(NA, as.matrix(d), NA), NA)
res <- 0

for(s in 1:100){
  ix10 <- NULL
  # all around >9 then 10
  ixAround <- t(sapply(2:11, function(rn)
    sapply(2:11, function(cn){
      all(m[ around(c(rn, cn)) ] >= 9, na.rm = TRUE)
    })))
  m[ 2:11, 2:11 ][ ixAround ] <- 9
  m <- m + 1
  while(TRUE){
    ixToFlash <- which(m > 9, arr.ind = TRUE)
    ixToFlash <- ixToFlash[ !(paste(ixToFlash[, 1], ixToFlash[, 2], sep = "_") %in% 
                                paste(ix10[, 1], ix10[, 2], sep = "_")), , drop = FALSE ]
    if(nrow(ixToFlash) == 0) break
    ix10 <- rbind(ix10, ixToFlash)
    ixAround <- do.call(rbind, lapply(seq(nrow(ixToFlash)), 
                                      function(i) around(ixToFlash[i, ])))
    for(n in seq(nrow(ixAround))){
      if(!(paste(ixAround[n, ], collapse = "_") %in% 
           paste(ix10[, 1], ix10[, 2], sep = "_")))
        m[ ixAround[n, , drop = FALSE] ] <- m[ ixAround[n, , drop = FALSE ] ] + 1
    }
  } # end while
  res <- res + sum(m > 9, na.rm = TRUE)
  m[ m > 9 ] <- 0
  
} # end for
res
# [1] 1613

#part 2 -----------------------------------------------------------------------
m <- rbind(NA, cbind(NA, as.matrix(d), NA), NA)
s = 0

while(TRUE){
  s <- s + 1
  ix10 <- NULL
  # all around >9 then 10
  ixAround <- t(sapply(2:11, function(rn)
    sapply(2:11, function(cn){
      all(m[ around(c(rn, cn)) ] >= 9, na.rm = TRUE)
    })))
  m[ 2:11, 2:11 ][ ixAround ] <- 9
  m <- m + 1
  
  while(TRUE){
    ixToFlash <- which(m > 9, arr.ind = TRUE)
    ixToFlash <- ixToFlash[ !(paste(ixToFlash[, 1], ixToFlash[, 2], sep = "_") %in% 
                                paste(ix10[, 1], ix10[, 2], sep = "_")), , drop = FALSE ]
    if(nrow(ixToFlash) == 0) break
    ix10 <- rbind(ix10, ixToFlash)
    ixAround <- do.call(rbind, lapply(seq(nrow(ixToFlash)), 
                                      function(i) around(ixToFlash[i, ])))
    for(n in seq(nrow(ixAround))){
      if(!(paste(ixAround[n, ], collapse = "_") %in% 
           paste(ix10[, 1], ix10[, 2], sep = "_")))
        m[ ixAround[n, , drop = FALSE] ] <- m[ ixAround[n, , drop = FALSE ] ] + 1
    }
  } # end while

  m[ m > 9 ] <- 0
  
  if(sum(m == 9, na.rm = TRUE) == 100) break
} # end for
s
# [1] 510
