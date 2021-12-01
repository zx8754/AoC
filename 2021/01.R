x <- as.integer(readLines("01_data.txt"))

#part 1
sum(diff(x) > 0)
# [1] 1709

#part 2
sum(diff(sapply(1:(length(x)-2), function(i) sum(x[ i:(i+2) ]))) > 0)
# [1] 1761
