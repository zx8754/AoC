x <- as.integer(readLines("01_data.txt"))

#part 1
s <- sapply(split(x, cumsum(is.na(x))), sum, na.rm = TRUE)
max(s)
# 69501 

#part 2
sum(sort(s, decreasing = TRUE)[1:3])
# [1] 202346
