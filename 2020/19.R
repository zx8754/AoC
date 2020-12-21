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
