d <- read.table("08_data.txt", sep = "|")

#0-9
svd <- setNames(c("abcefg", "cf", "acdeg", "acdfg", "bcdf",
                  "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"), 0:9)
#1,4,7,8
u <- nchar(svd)[ c(1,4,7,8) + 1 ]

#part 1 -----------------------------------------------------------------------
sum(sapply(d$V2, function(i){
  sum(nchar(strsplit(i, " ", fixed = TRUE)[[1]]) %in% u)
}))
# [1] 284

#part 2 -----------------------------------------------------------------------
# stole the logic from https://www.reddit.com/r/adventofcode/comments/rbvpui/2021_day_8_part_2_my_logic_on_paper_i_used_python/
sl <- function(x) sort(strsplit(trimws(x), "")[[1]])
sw <- function(x){unname(
  sapply(strsplit(trimws(x), split = " ", fixed = TRUE)[[1]],
         function(i) paste(sl(i), collapse = "")))}

out <- sapply(seq(nrow(d)), function(i){
  #i=1
  v1 <- sw(d$V1[ i ])
  v2 <- sw(d$V2[ i ])
  res <- vector("character", 10)
  #1478 -----------------------------------------------------------------------
  res[ 2 ] <- v1[ nchar(v1) == 2 ]
  res[ 5 ] <- v1[ nchar(v1) == 4 ]
  res[ 8 ] <- v1[ nchar(v1) == 3 ]
  res[ 9 ] <- v1[ nchar(v1) == 7 ]
  # find L: 4 minus 1
  diff41 <- setdiff(sl(res[ 5 ]), sl(res[ 2 ]))
  #nchar 5: 2,3,5 -------------------------------------------------------------
  #3
  x <- v1[ nchar(v1) == 5 ]
  res[ 4 ] <- x[ sapply(x, function(i){
    length(intersect(sl(res[ 2 ]), sl(i))) == 2}) ]
  #5
  x <- x[ x != res[ 4 ] ]
  ix <- sapply(x, function(i) sum(diff41 %in% sl(i)) == 2)
  res[ 6 ] <- x[ ix ]
  #2
  res[ 3 ] <- x[ !ix ]
  #nchar 6: 0,6,9 -------------------------------------------------------------
  x <- v1[ nchar(v1) == 6 ]
  #9 contains 4
  res[ 10 ] <- x[ sapply(x, function(i) all(sl(res[ 5 ]) %in% sl(i))) ]
  #6 L
  x <- x[ x != res[ 10 ] ]
  res[ 7 ] <- x[ sapply(x, function(i) sum(diff41 %in% sl(i)) == 2) ]
  # 0
  res[ 1 ] <- setdiff(v1, res)
  
  # match
  as.integer(paste(match(v2, res) - 1, collapse = ""))
})

sum(out)
# [1] 973499
