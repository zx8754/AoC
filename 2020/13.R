x <- readLines("13_data.txt")
#x <- readLines("13_sample.txt")
tm <- as.integer(x[ 1 ])

# part 1 ------------------------------------------------------------------
b <- as.integer(unlist(strsplit(x[ 2 ], ",")))
b <- b[ !is.na(b) ]

res <- sapply(b, function(i){
  #i=b[1]
  g <- rep(".", tm + max(b))
  g[ seq(i, length(g), i) ] <- "D"
  g <- setNames(g[ tm:length(g) ], tm:length(g))
  which(g == "D")[ 1 ]
})
b[ which.min(res) ] * (as.integer(names(res)[ which.min(res) ]) - tm)
# [1] 4938

# part 2 ------------------------------------------------------------------
# Giving up....

# Other solutions:
# - https://github.com/AdroMine/AdventOfCode2020/blob/main/Day13/solution.R
# - https://tanho63.github.io/advent_of_code/2020/Day-13


# v2 FAIL
# b <- unlist(strsplit(x[ 2 ], ","))
# 
# #b <- unlist(strsplit("1789,37,47,1889", ","))
# #b <- unlist(strsplit("17,x,13,19", ",")) #3417
# d <- cbind.data.frame(b = b, s = seq_along(b) - 1)
# d <- d[ d$b != "x", ]
# d$b <- as.integer(d$b)
# 
# res <- vector()
# f <- i <- d$b[ 1 ]
# for(rn in seq(nrow(d))){
#   #rn=2
#   dd <- d[ 1:rn, ]
#   t1 <- Sys.time()
#   test = TRUE
#   #i <- pracma::Lcm(f, max(dd$b))
#   #res = 0
#   while(test){
#     print(i); flush.console()
#     if(
#       sum(apply(dd, 1,  function(j){
#         (i + j[ 2 ]) %% j[ 1 ] == 0})) == rn 
#     ){res <- c(res, i) 
#     test = FALSE
#     break}
#     i = i + f
#     }
#   f <- i
#   Sys.time() - t1
#   Sys.sleep(5)
# }
# 
# # v1 Brute force, works for small examples, but not bigger inputs...
# #b <- unlist(strsplit("17,x,13,19", ",")) #3417
# b <- unlist(strsplit("17,x,13", ",")) #?
# d <- cbind.data.frame(b = b, s = seq_along(b) - 1)
# d <- d[ d$b != "x", ]
# d$b <- as.integer(d$b)
# 
# t1 <- Sys.time()
# test = TRUE
# #i <- f <- max(d$b) 
# f <- i <- d$b[1]
# #i <- pracma::Lcm(f, max(d$b))
# nr <- nrow(d)
# res = 0
# while(test){
#   print(i); flush.console()
#   if(
#     sum(apply(d, 1,  function(j){
#       (i + j[ 2 ]) %% j[ 1 ] == 0})) == nr 
#   ){res <- i 
#   test = FALSE
#   break}
#   i = i + f
# }
# Sys.time() - t1
# 
