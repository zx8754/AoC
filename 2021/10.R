d <- readLines("10_data.txt")

#part 1 -----------------------------------------------------------------------
removeComplete <- function(x){
  g <- x
  check = TRUE
  while(check){
    g <- gsub("()", "", g, fixed = TRUE)
    g <- gsub("{}", "", g, fixed = TRUE)
    g <- gsub("<>", "", g, fixed = TRUE)
    g <- gsub("[]", "", g, fixed = TRUE)
    if(!(any(grepl("()", g, fixed = TRUE) |
             grepl("{}", g, fixed = TRUE) |
             grepl("<>", g, fixed = TRUE) |
             grepl("[]", g, fixed = TRUE)))) check = FALSE
  }
  g
}

dropIncomlete <- function(x){
  ix <- sapply(strsplit(x, ""), function(i){
    all(i %in% c("{","(", "<", "[")) |
      all(i %in% c("}",")", ">", "]"))
  })
  x[!ix]
}

# illegal combos
x <- do.call(paste0, expand.grid(c("{", "(", "[", "<"), c("}", ")", "]", ">")))
x <- x[ !x %in% c("{}", "[]", "{}", "<>") ]

out <- d
out <- removeComplete(out)
out <- dropIncomlete(out)
check = TRUE
res <- NULL
while(check){
  r <- sapply(x, function(i) grepl(i, out, fixed = TRUE))  
  ixOut <- which(rowSums(r) == 1)
  ixB <- x[ apply(r, 1, which) ]
  for(i in seq_along(ixOut)){
    #i=3
    out[ ixOut[i] ] <- gsub(ixB[i], "", out[ ixOut[i] ], fixed = TRUE)
  }
  out <- removeComplete(out)
  out <- dropIncomlete(out)
  
  res <- rbind(res, r)
  if(sum(r) == 0 | length(out) == 0) check = FALSE
}

res <- colSums(res)
names(res) <- substr(names(res), 2, 2)
res <- aggregate(values ~ ind, data = stack(res), sum)
# ): 3 points.
# ]: 57 points.
# }: 1197 points.
# >: 25137 points.
res <- res[ order(res$ind), ]
sum(res$values * c(1197, 3, 57, 25137))
# [1] 392097

#part 2 -----------------------------------------------------------------------
d <- readLines("10_data.txt")
out <- removeComplete(d)
ix <- sapply(strsplit(out, ""), function(i){
    all(i %in% c("{","(", "<", "[")) |
      all(i %in% c("}",")", ">", "]"))
  })
  
out <- out[ ix ]
score <- setNames(c(1,2,3,4), c("(", "[", "{", "<"))

median(sapply(out, function(i){
  #i=out[1]
  res <- 0
  for(x in rev(strsplit(i, "")[[1]])){
    res <- res * 5 + score[ x ]
  }
  res
}))
# [1] 1047744
