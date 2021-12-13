d <- read.table("12_data.txt", sep = "-")

library(igraph)
g <- graph_from_data_frame(d, directed = FALSE)
plot(g)

#part 1 -----------------------------------------------------------------------

#part 2 -----------------------------------------------------------------------
