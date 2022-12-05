x <- readLines("03_data.txt")

# part1 -------------------------------------------------------------------
n <- nchar(x)

s1 <- strsplit(substring(x, 1, n/2), "")
s2 <- strsplit(substring(x, n/2 + 1), "")

priority <- setNames(1:52, c(letters, LETTERS))
sum(priority[ unlist(Map(intersect, s1, s2)) ])
# [1] 7795

# part2 -------------------------------------------------------------------
nsack <- 
x1 <- split(x, rep(1:(length(x)/3), each = 3))

sum(
  sapply(x1, function(i){
  ss <- strsplit(i, "")
  priority[ intersect(intersect(ss[[ 1 ]], ss[[ 2 ]]), ss[[ 3 ]]) ]
}))
#[1] 2703
