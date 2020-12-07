x <- readLines("07_data.txt")
d <- strsplit(x, " contain ")

# part1 - igraph magic ----------------------------------------------------
from <- sapply(d, function(i) gsub(" bags| bag", "", i[ 1 ]))
to <- sapply(d, function(i){ 
  b <- gsub(" bags| bag", "", unlist(strsplit(i[ 2 ], ", "))) 
  b <- gsub(".", "", b, fixed = TRUE)
  b <- gsub("[0-9] ", "", b)
  b[ b!= "no other" ]
  })
ix <- lengths(to) > 0
from <- from[ ix ]
to <- to[ ix ]

library(igraph)

g <- graph_from_data_frame(do.call(rbind.data.frame, mapply(cbind, from, to)))
length(subcomponent(g, v = "shiny gold", mode = "in")) - 1
# [1] 246


# part2 - recursive pain --------------------------------------------------
from <- sapply(d, function(i) gsub(" bags| bag", "", i[ 1 ]))
to <- sapply(d, function(i){ 
  b <- gsub(" bags| bag", "", unlist(strsplit(i[ 2 ], ", "))) 
  b <- gsub(".", "", b, fixed = TRUE)
  b <- b[ b!= "no other" ]
  rep(gsub("[0-9] ", "", b), as.integer(gsub("[a-z]", "", b)))
})
ix <- lengths(to) > 0
from <- from[ ix ]
to <- to[ ix ]

foo <- function(from, to, s){
  ix <- na.omit(match(s, from))
  if(length(ix) == 0) { return(1) } else { 
    return(length(unlist(to[ ix ])) + foo(from, to, unlist(to[ ix ])))  }
  }

foo(from, to, "shiny gold") - 1
# [1] 2976
