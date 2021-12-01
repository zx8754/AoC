x <- readLines("19_data.txt")
#x <- readLines("19_sample.txt")

rules <- split(x, cumsum(x == ""))[[ 1 ]]

ss <- sapply(rules, function(i){
  d <- gsub('"', '', unlist(strsplit(i, ":")), fixed = TRUE)
  rn <- strsplit(d, ":")[[1]]
  nn <- lapply(strsplit(trimws(strsplit(d, ":")[[2]]), " | ", fixed = TRUE), 
               function(j){ strsplit(j, split = " ", fixed = TRUE) })
  nn
  #list(rn = rn, nn = nn)
})

f1 <- which(sapply(ss, function(i){
  all(!grepl("[0-9]", unlist(i)))
}))

ix <- unname(f1[ 1 ])
ixL <- unname(unlist(ss[ ix ]))

res <- ss
cc <- integer()

while(TRUE){
  res <- lapply(res, function(i){
    lapply(i, function(j){
      ifelse(j == (ix - 1), ixL, j)
    })
  })
  
  cc <- c(cc, ix)
  
  f1 <- which(sapply(res, function(i){
    all(!grepl("[0-9]", unlist(i)))
  }))
  f1 <- f1[ !f1 %in% cc ][ 1 ]
  ix <- unname(f1[ 1 ])
  ixL <- unname(unlist(ss[ ix ]))
}


library(igraph)
g <- graph_from_data_frame(ss, directed = FALSE)

plot(g)



# failing... gave up...

# x <- readLines("19_data.txt")
# #x <- readLines("19_sample.txt")
# 
# rules <- split(x, cumsum(x == ""))[[ 1 ]]
# # rules1 <- rules[ grepl('"', rules, fixed = TRUE) ]
# # rules2 <- rules[ !grepl('"', rules, fixed = TRUE) ]
# 
# ss <- do.call(rbind, lapply(rules, function(i){ 
#   d <- gsub('"', '', unlist(strsplit(i, ":")), fixed = TRUE)
#   rn <- strsplit(d, ":")[[1]]
#   nn <- trimws(strsplit(d, ":")[[2]])
#   data.frame(rn = rn, nn = nn)
# }))
# 
# cc <- character()
# # pp <- ss[ ss$nn == rr, "rn" ]
# while(any(grepl("[0-9]", ss$nn))){
#   rr <- ss$nn[ grepl("[ab]", ss$nn) & !grepl("[0-9]", ss$nn) & !(ss$nn %in% cc) ]
#   pp <- ss[ ss$nn == rr, "rn"]
#   rr <- strsplit(rr, " | ", fixed = TRUE)
#   for(i in seq_along(rr)){
#     #i=1
#     for(j in rr[[ i ]]){
#       #j = rr[[ i ]][1]
#     ss$nn <- gsub(pp[ i ], j, ss$nn, fixed = TRUE)
#     cc <- c(cc, j)
#     }
#     }
# }
