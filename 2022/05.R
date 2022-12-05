# Data --------------------------------------------------------------------
crates <- read.fwf("05_data.txt", widths = rep(4, 9), comment.char = "m")
#crates <- read.fwf("05_data.txt", widths = rep(4, 3), comment.char = "m")
crates <- head(crates, -3)
crates <- lapply(crates, function(i){
  x <- trimws(i)
  x <- x[ x != "" ]
  substring(x, 2, 2)
})

moves <- read.table("05_data.txt", comment.char = "[", fill = TRUE)
moves <- moves[ moves$V1 == "move", ]

# udf ---------------------------------------------------------------------
f <- function(cc, mm, rev = TRUE){
  for(i in seq_len(nrow(mm))){
    #i=1
    n = mm[i, 2]
    from = mm[i, 4]
    to = mm[i, 6]
    m = head(cc[[ from ]], n)
    if(rev) m <- rev(m)
    cc[[ to ]] <- c(m, cc[[ to ]])
    cc[[ from ]] <- tail(cc[[ from ]], -n)
  }
  paste(unlist(sapply(cc, head, 1)), collapse = "")
}

# part1 -------------------------------------------------------------------
f(crates, moves)
# [1] "DHBJQJCCW"
# part2 -------------------------------------------------------------------
f(crates, moves, rev = FALSE)
# [1] "WJVRLSJJT"
