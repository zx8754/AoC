x <- read.table("02_data.txt")

#part 1
ss <- aggregate(V2 ~ V1, data = x, sum)
ss[ ss$V1 == "forward", 2 ] * (ss[ ss$V1 == "down", 2] - ss[ ss$V1 == "up", 2])
# [1] 2215080

#part 2
a = h = d = 0
for(i in seq(nrow(x))){
  type <- x$V1[ i ]
  n <- x$V2[ i ]
  if(type == "forward"){
    h <- h + n
    d <- d + a * n
  } else {
    a <- a + n * setNames(c(1, -1), c("down", "up"))[ type ]
  }}
h * d
# [1] 1864715580
