x <- as.integer(readLines("01_data.txt"))

#2 combo
prod(x[(2020 - x) %in% x])
# [1] 138379

#3 combo
prod(x[ (2020 - x) %in% rowSums(expand.grid(x, x)) ])
# [1] 85491920
