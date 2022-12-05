x <- read.table("02_data.txt")

# part1 -------------------------------------------------------------------
rps <- c("Rock", "Paper", "Scissors")
a1 <- setNames(c("Rock", "Paper", "Scissors"), c("A", "B", "C"))
a2 <- setNames(c("Rock", "Paper", "Scissors"), c("X", "Y", "Z"))

score <- setNames(1:3, c("Rock", "Paper", "Scissors"))

s = setNames(1:3, c("Rock", "Paper", "Scissors"))
g <- cbind(expand.grid(rps, rps),
           res = c(3,0,6,6,3,0,0,6,3))
g <- cbind(g, score = s[ g$Var2 ])

res <- merge(data.frame("Var1"=a1[ x$V1 ], "Var2" = a2[ x$V2 ]), g)

sum(rowSums(res[, 3:4]))
#[1] 13675

# part2 -------------------------------------------------------------------
a3 <- setNames(c(0,3,6), c("X", "Y", "Z"))
# X means you need to lose
# Y means you need to draw
# Z means you need to win
res2 <- merge(data.frame("Var1"=a1[ x$V1 ], "res" = a3[ x$V2 ]), g[, -2 ])
sum(rowSums(res2[, 2:3]))
# [1] 14184
