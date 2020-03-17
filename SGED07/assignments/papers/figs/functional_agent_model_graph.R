library(ggsci)
library(tidyverse)
library(ggpubr)
library(ggraph)
library(igraph)
library(wesanderson)

model_gr <- graph_from_literal(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 2 -+ 6, 4 -+ 6, 8 -+ 6,  9 -+ 6)
model_gr <- set_vertex_attr(model_gr, "fun", index = V(model_gr), c(0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1))

ggraph(model_gr, layout = 'linear') +
  geom_edge_arc(arrow = arrow(length = unit(3, 'mm')),
                start_cap = circle(5, 'mm'),
                end_cap = circle(5, 'mm'),
                strength = 0.5, # bend of curve
                fold = FALSE) + 
  coord_fixed() +
  geom_node_point(size = 10, aes(col = as.factor(fun))) +
  geom_node_text(aes(label = name)) +
  theme_graph() +
  scale_color_manual(values = wes_palette("Darjeeling2")) +
  theme(legend.position='none')

model_gr1 <- graph_from_literal(1 -+ 2, 2 -+ 3, 3 -+ 4) 

g1 <- ggraph(model_gr1) +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')),
                start_cap = circle(5, 'mm'),
                end_cap = circle(5, 'mm'),
                strength = 0.5, # bend of curve
                fold = FALSE) + 
  geom_node_point(size = 10, color = "#0073C299") +
  geom_node_text(aes(label = name)) +
  theme_graph() +
  theme(legend.position='none')

model_gr2 <- graph_from_literal(1 -+ 2, 1 -+ 3, 1 -+ 4) 

g2 <- ggraph(model_gr2) +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')),
                start_cap = circle(7, 'mm'),
                end_cap = circle(7, 'mm'),
                strength = 0.5, # bend of curve
                fold = FALSE) + 
  geom_node_point(size = 10, color = "#0073C299") +
  geom_node_text(aes(label = name)) +
  theme_graph() +
  theme(legend.position='none')

ggarrange(g1, g2, nrow = 1, ncol = 2, labels = c("A", "B"))
