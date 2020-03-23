library(tidyverse)
library(ggpubr)

# output decline bwetween .95 and 90 in quality, by number of tasks
tb <- tibble(tasks = 1:100, output_95 = 0.95^(tasks)*tasks, output_90 = 0.9^(tasks)*tasks) %>%
  mutate(diff_ratio = output_95 / output_90,
         diff_abs = output_95 - output_90)

# output by quality for 10 task process
 quality <- seq(0.50, 1, 0.01)
 tasks <- rep(10, length(quality))
 output_tb <- tibble(quality, tasks) %>%
 mutate(output = quality^(tasks)*tasks)

p5 <- ggline(output_tb,
       numeric.x.axis = TRUE,
       x = "quality",
       y = "output",
       plot_type = "l",
       xlab = "z",
       ylab = "Output") %>%
  ggpar(font.tickslab = 10, font.x = 10, font.y = 10) +
  geom_segment(aes(x = .95, y = 0, xend = 0.95, yend = 5.99), linetype = "longdash", size = 0.4) +
  geom_segment(aes(x = 0.5, y = 5.99, xend = 0.95, yend = 5.99), linetype = "longdash", size = 0.4) +
  geom_segment(aes(x = 0.90, y = 0, xend = 0.90, yend = 3.49), linetype = "dotted", size = 0.4) +
  geom_segment(aes(x = 0.5, y = 3.49, xend = 0.90, yend = 3.49), linetype = "dotted", size = 0.4)


# percentage of output lost by worker quality for 4 different task levels
quality <- seq(0.75, 1, 0.01)

tasks <- c(rep(5, length(quality)), rep(10, length(quality)), rep(25, length(quality)))

 scaled_output_tb <- tibble(qual = rep(quality, 3), tasks) %>%
 mutate(output = qual^(tasks)*tasks) %>%
   mutate(rescaled_output = output/tasks) %>%
   mutate(tasks = as.factor(tasks))

p6 <- ggline(scaled_output_tb,
             numeric.x.axis = TRUE,
             #group = "tasks",
             x = "qual",
             y = "rescaled_output",
             linetype = "tasks",
             plot_type = "l",
             xlab = "z",
             ylab = "% of output",
             palette = "jco") %>%
  ggpar(font.tickslab = 10, font.x = 10, font.y = 10, font.legend = 10, legend = "top", legend.title = "# of inputs") +
  theme(legend.position = c(0.2, 0.8))

ggarrange(p5, p6, ncol = 2, nrow = 1, labels = c("A", "B"), font.label = list(size = 10, face = "bold"))

ggsave(
  filename = "/home/post/university/SGED07/assignments/papers/final_folder/figures/z.pdf",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 18.4,
  height = 8,
  units = c("cm"),
  dpi = "print",
  limitsize = TRUE
)

