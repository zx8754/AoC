# data ------------------------------------------------------------------------
d <- readLines("13_data.txt")

pos <- t(sapply(1:(which(d == "") - 1), function(i) 
  as.integer(unlist(strsplit(d[ i ], ",")))))
pos <- pos + 1
folds <- gsub("fold along ", "", d[(which(d == "") + 1) : length(d)])
paper <- matrix(FALSE, ncol = max(pos[, 1]), nrow = max(pos[, 2]))
paper[ pos[, 2:1] ] <- TRUE

# udf -------------------------------------------------------------------------
fold <- function(m, f){
  x <- unlist(strsplit(f, "="))
  xy <- x[ 1 ]
  n <- as.integer(x[ 2 ]) + 1
  r <- m
  #if y rotate -90
  if(xy == "y") r <- t(r)[ ncol(r):1, ]
  
  right <- ncol(r):(n + 1)
  left <- (n - length(right)):(n - 1)
  r[, left ] <- r[, left ] | r[, right ]
  r <- r[, seq_len(n - 1) ]
  
  #if y rotate +90
  if(xy == "y") r <- t(r[ nrow(r):1, ])
  #return
  r
}

foldPlot <- function(d) image(t(d[ nrow(d):1, ]), xaxt = "n", yaxt = "n", 
                              axes = FALSE, col = c("white", "#B3000C"))


#part 1 -----------------------------------------------------------------------
out <- fold(paper, folds[ 1 ])
sum(out > 0)
# [1] 618

#part 2 -----------------------------------------------------------------------
out <- paper
for(f in folds) out <- fold(out, f)
out[ out > 0 ] <- 1

foldPlot(out)
# ALREKFKU

# viz -------------------------------------------------------------------------
# jpeg
out <- paper
for(f in folds) out <- fold(out, f)
jpeg("13.jpeg", width = 900, height = 300)
foldPlot(out)
dev.off()

# gif
library(animation)
# ImageMagick required
saveGIF({
  out <- paper
  foldPlot(out)
  for(f in folds) {
    out <- fold(out, f)
    foldPlot(out)
    }}, movie.name = "13.gif")
