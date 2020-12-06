x <- readLines("06_data.txt")
d <- split(x, cumsum(x == ""))
d <- lapply(d, function(i) i[ i != "" ])

#part1
sum(sapply(d, function(i) length(unique(unlist(strsplit(i, ""))))))
# [1] 6504

#part2
sum(
  sapply(d, function(i){
    s <- strsplit(i, "")
    if(length(i) == 1){ lengths(s) } else { length(Reduce(intersect, s)) }
    })
  )
# [1] 3351
