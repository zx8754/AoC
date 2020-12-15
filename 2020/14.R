library(data.table)
x <- read.table("14_data.txt", sep = " ")
#x <- read.table("14_sample1.txt", sep = " ")

# part 1 ------------------------------------------------------------------
b36MaskInt <- function(x, m){
  b <- c(rep(0, 4), as.integer(rev(intToBits(x))))
  sum(2^(which(rev(ifelse(m == b | m == "X", b, m)) == 1) - 1))
}

dd <- rbindlist(
  lapply(split(x, cumsum(x$V1 == "mask")), function(i){
    mask = strsplit(i$V3[ 1 ], "")[[ 1 ]]
    data.frame(
      mem = gsub("\\D", "", tail(i$V1, -1)),
      v = sapply(tail(i$V3, -1), function(j) b36MaskInt(j, mask)))
  }), idcol = "grp")

#https://stackoverflow.com/a/24558696/680068
dd[, ix := seq_along(.I), by = mem ]
print(dd[ dd[, .I[which.max(ix)], by = mem ]$V1 ][, sum(v)], digits = 22)
#[1] 4886706177792

# part 2 ------------------------------------------------------------------
# fail: works with example data, but fails with real data...
# x <- read.table("14_sample2.txt", sep = " ")
# x <- read.table("14_data.txt", sep = " ")


# b36MaskIntCombn <- function(x, m){
#   #x=42; m=strsplit("000000000000000000000000000000X1001X", "")[[ 1 ]]
#   #x=mem[1]; m=mask
#   b <- c(rep("0", 4), as.integer(rev(intToBits(x))))
#   bm <- ifelse(m %in% c("1", "X"), m, b)
#   #cbind(b, m, bm)
#   bmX <- which(bm == "X")
#   bm0 <- bm; bm0[ bmX ] <- 0
#   sort(apply(do.call(cbind,
#                 c(list(matrix(bm0, ncol = 1)),
#                   lapply(seq(length(bmX)), function(i){
#                     #combn(bmX, i)})
#                     
#                     combn(bmX, i, FUN = function(X){
#                       ones <- bm0
#                       ones[ X ] <- 1
#                       #ones[ setdiff(bmX, X) ] <- 0
#                       ones})
#                   }))), 2, function(bmCmbn){
#                     sum(2^(which(rev(bmCmbn) == "1") - 1))
#                   }))
#   }
# 
# dd <- rbindlist(
#   lapply(split(x, cumsum(x$V1 == "mask")), function(i){
#     
#     #i=split(x, cumsum(x$V1 == "mask"))[[1]]
#     
#     mask = strsplit(i$V3[ 1 ], "")[[ 1 ]]
#     mem = as.integer(gsub("\\D", "", tail(i$V1, -1)))
#     data.frame(
#       mem = mem,
#       newMem = c(sapply(mem, function(j) b36MaskIntCombn(j, mask))),
#       v = as.integer(tail(i$V3, -1)))
#   }))
# 
# dd
# 
# #[1] 3358975932677 fail
# setorderv(dd, c("mem", "newMem"))
# dd[, ix := seq_along(.I), by = newMem ]
# print(dd[ dd[, .I[which.max(ix)], by = newMem ]$V1 ][, sum(v)], digits = 22)
# 
# 
# # 3343308395565 fail
# setorderv(dd, "newMem")
# dd[, ix := seq_along(.I), by = newMem ]
# print(dd[ dd[, .I[which.max(ix)], by = newMem ]$V1 ][, sum(v)], digits = 22)
