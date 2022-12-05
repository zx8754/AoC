x <- read.table("04_data.txt", sep = ",")

# part1 -------------------------------------------------------------------
sum(
  apply(x, 1, function(i){ 
    s1 <- as.integer(unlist(strsplit(i[ 1 ], "-")))
    s2 <- as.integer(unlist(strsplit(i[ 2 ], "-")))
    s3 <- s1[ 1 ]:s1[ 2 ]
    s4 <- s2[ 1 ]:s2[ 2 ]
    length(setdiff(s3, s4)) == 0 |
      length(setdiff(s4, s3)) == 0
  }))
# [1] 494

# part2 -------------------------------------------------------------------
sum(
  apply(x, 1, function(i){ 
    s1 <- as.integer(unlist(strsplit(i[ 1 ], "-")))
    s2 <- as.integer(unlist(strsplit(i[ 2 ], "-")))
    s3 <- s1[ 1 ]:s1[ 2 ]
    s4 <- s2[ 1 ]:s2[ 2 ]
    length(intersect(s3, s4)) > 0
  }))
# [1] 833
