library(igraph)
library(ggsci)



node_s <- 35
vs <- 6
colors <- c("#0073C299", "#86868699")
arrow_s <- 0.5
arrow_w <- 0.5
label_s <- 30
cex <- 3.5
marg <- 0

g1 <- graph_from_literal(1 -+ 2, 2 -+ 3, 3 -+ 4) 
g2 <- graph_from_literal(4 -+ 1, 3 -+ 1, 2 -+ 1) 
g3 <- graph_from_literal(1 -+ 3, 2 -+ 3, 3 -+ 4) 


par(mfrow = c(1, 3)) # Create a 2 x 2 plotting matrix

plot(
  g1,
  vertex.label.cex = cex,
  vertex.color = colors[1],
  vertex.size = node_s,
  layout = layout_nicely,
  edge.arrow.size=arrow_s,
  edge.arrow.width=arrow_w,
  edge.lty=c("solid"),
  edge.color = "black",
  vertex.label.family=2,
  vertex.label.color = "black",
  margin =marg
  )

plot(
y
  g2,
  vertex.label.cex = cex,
  vertex.color = colors[1],
  vertex.size = node_s,
  layout = layout_as_star,
  edge.arrow.size=arrow_s,
  edge.arrow.width=arrow_w,
  edge.lty=c("solid"),
  edge.color = "black",
  vertex.label.family=2,
  vertex.label.color = "black",
  margin = 0
)

plot(
  g3,
  vertex.label.cex = cex,
  vertex.color = colors[1],
  vertex.size = node_s,
  layout = layout_nicely,
  edge.arrow.size=arrow_s,
  edge.arrow.width=arrow_w,
  edge.lty=c("solid"),
  edge.color = "black",
  vertex.label.family=2,
  vertex.label.color = "black",
  margin = marg
)



