x <- as.list(read.fwf("12_data.txt", widths = c(1, 10), col.names = c("d", "v")))
#x <- as.list(read.fwf("12_sample.txt", widths = c(1, 10), col.names = c("d", "v")))

# part 1 ------------------------------------------------------------------
degree <- c("0" = "E", "90" = "N", "180" = "W", "270" = "S")
p <- d <- "E"
r = 0
res <- setNames(rep(0, 4), c("E", "W", "N", "S"))
for(i in seq(length(x$d))){
  print(paste(x$d[ i ], x$v[ i ], collapse = " ")); flush.console()
  d <- x$d[ i ]
  v <- x$v[ i ]
  if(d == "F"){ res[ p ] <- res[ p ] + v 
  } else if(d %in% names(res)){ res[ d ] <- res[ d ] + v 
  } else if(d == "L"){
    r <- (r + v) %% 360
    p <- degree[ as.character(r) ]
  } else if(d == "R"){
    r <- (r - v)
    if( r < 0 ) r <- 360 + r
    r <- r %% 360
    p <- degree[ as.character(r) ]
  }
}
abs(res[ "E" ] - res[ "W" ]) + abs(res[ "N" ] - res[ "S" ])
#    E 
# 1319 

# part 2 ------------------------------------------------------------------
# will fix laterrrrrrrrr

# degree <- c("0" = "E", "90" = "N", "180" = "W", "270" = "S")
# p <- d <- "E"
# r = 0
# w = c("E" = 10, "S" = 0, "W" = 0, "N" = 1)
# wp = w[ w != 0 ]# "NE" #SE SW NW
# res <- setNames(rep(0, 4), c("E", "W", "N", "S"))
# for(i in seq(length(x$d))){
#   #for(i in 1:3){
#   print(paste(x$d[ i ], x$v[ i ], collapse = " ")); flush.console()
#   #i=3
#   d <- x$d[ i ]
#   v <- x$v[ i ]
#   if(d %in% names(res)){
#     w[ d ] <- w[ d ] + v
#   } else if(d == "F"){ 
#     for(ww in names(wp)){ res[ ww ] <- res[ ww ] + w[ ww ] * v }
#   } else if(d %in% names(res)){
#     w[ d ] <- w[ d ] + v
#   } else if(d == "L"){
#     r <- (r + v) %% 360
#     r <- r %% 360
#     rr <- r/90
#     w[ 1:4 ] <- unname(rep(w, 2)[ 5:8 - rr ])
#     wp <- w[ w != 0 ]
#   } else if(d == "R"){
#     r <- (r - v)
#     if( r < 0 ) r <- 360 + r
#     r <- r %% 360
#     rr <- r/90
#     w[ 1:4 ] <- unname(rep(w, 2)[ 1:4 + rr ])
#     wp <- w[ w != 0 ]
#   }
#   #table(x$v[ x$d == "R"])  
# }
# abs(res[ "E" ] - res[ "W" ]) + abs(res[ "N" ] - res[ "S" ])
