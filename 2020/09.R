x <- as.numeric(readLines("09_data.txt"))

# part 1 ------------------------------------------------------------------
foo <- function(x, n){
  for(i in seq.int(length(x) - 5)){
    ix <- seq(i, i + n)
    a <- x[ ix[ 1:n ] ]
    b <- x[ ix[ n + 1 ] ]
    if(sum((b - a) %in% a) < 2) { break }
    }
  b
}

foo(x, n = 25)
# [1] 552655238

# part 2 ------------------------------------------------------------------
part1 <- foo(x, n = 25)
l <- x[ 1:(which(x == part1) - 1)  ]
for(i in seq_along(l)){
  out = l[ i ]
  j = seq(i + 1, length(l))
  n = 1
  while(out < part1){
    out <- out + l[ j[ n ] ]
    if(out == part1) {
      target <- c(i, n, out)
      break}
    n <- n + 1
  }
}

sum(range(l[ target[1]:(target[1] + target[2]) ]))
# [1] 70672245
