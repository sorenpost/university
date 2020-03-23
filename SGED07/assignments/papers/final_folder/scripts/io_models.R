library(ggE)
library(tidyverse)
library(ggpubr)
library(ggraph)
library(igraph)
library(gridExtra)

node_size <- 15
cc <- 7
text_s <- 8

model_gr1 <- graph_from_literal(1 -+ 2, 2 -+ 3, 3 -+ 4) 

g1 <- ggraph(model_gr1) +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')),
                 start_cap = circle(cc, 'mm'),
                 end_cap = circle(cc, 'mm'),
                 strength = 0.5, # bend of curve
                 fold = FALSE) + 
  geom_node_point(size = node_size, color = "#0073C299") +
  geom_node_text(aes(label = name), size = text_s) +
  theme_graph() +
  theme(legend.position='none')

model_gr2 <- graph_from_literal("1" -+ "2", "3" -+ "2", "3", "4") 

g2 <- ggraph(model_gr2) +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')),
                 start_cap = circle(cc, 'mm'),
                 end_cap = circle(cc, 'mm')) +
  geom_node_point(size = node_size, color = "#0073C299") +
  geom_node_text(aes(label = name), size = text_s) +
  theme_graph() +
  theme(legend.position='none')

grid.arrange(g1, g2, nrow = 1)

                        
