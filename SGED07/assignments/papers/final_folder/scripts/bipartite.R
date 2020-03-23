# bipartite

library(igraph)
library(tibble)

# Create edges
V1 <- c("C1", "C1", "C1", "C2", "C2", "C3")
V2 <- c("P1", "P2", "P3", "P2", "P3", "P3")
tb_bi <-tibble(V1, V2)

# Create graph
g1 <- graph_from_data_frame(tb_bi, directed = FALSE)

# create labels
V(g1)$label <- V(g1)$name

# set type (type sets the two different graphs in the bipartite)
V(g1)$type <- FALSE

V(g1)[name %in% c("P1", "P2", "P3")]$type <- TRUE

# for color version: V(g1)$color <- ifelse(V(g1)$type == 1, "#EFC00099", "#0073C299")
V(g1)$color <- ifelse(V(g1)$type == 1, "#000000", "#FFFFFF")
V(g1)$shape <- ifelse(V(g1)$type == 1, "square", "square")
V(g1)$size <- 35
V(g1)$font <- "sans"


# tripartite
V1 <-   c("C1", "C1", "C1", "C2", "C2", "C3")
V2 <-   c("A1", "A2", "A3", "A2", "A3", "A3")
V2.2 <- c("A1", "A2", "A2", "A3", "A3", "A3")
V3 <-   c("P1", "P1", "P2", "P1", "P2", "P3")
tb_tri1 <- tibble(V1, V2)
tb_tri2 <- tibble(V2.2, V3)
  
gtri1 <- graph_from_data_frame(tb_tri1, directed = FALSE)
gtri2 <- graph_from_data_frame(tb_tri2, directed = FALSE)

g3 <- gtri1 + gtri2

V(g3)$label <- V(g3)$name
V(g3)$type <- 1
V(g3)[name %in% c("A1", "A2", "A3")]$type <- 2
V(g3)[name %in% c("P1", "P2", "P3")]$type <- 3

# For color version: V(g3)$color <- ifelse(V(g3)$type == 1, "#0073C299", ifelse(V(g3)$type == 2, "#86868699", "#EFC00099"))
V(g3)$color <- ifelse(V(g3)$type == 1, "#FFFFFF", ifelse(V(g3)$type == 2, "#86868699", "#000000"))
V(g3)$shape <- ifelse(V(g3)$type == 1, "square", ifelse(V(g3)$type == 2, "circle", "square"))
V(g3)$size <- 35
V(g3)$font <- "sans"

layout <- layout_with_sugiyama(g3, layers = V(g3)$type)$layout

## arrange
par(mfrow = c(1, 2)) # Create a 2 x 2 plotting matrix

plot(g3,
     layout = layout,
     vertex.label.family=2,
     vertex.label.color = c("black", "black", "black", "black", "black", "black", "white", "white", "white"),
     edge.width = 2,
     edge.color = "black")


plot(g1, layout = layout_as_bipartite, 
     vertex.label.family=2,
     vertex.label.color = c("black", "black", "black", "white", "white", "white"),
     edge.width = 2,
     edge.color = "black")
 






