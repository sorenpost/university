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
       xlab = "Quality",
       ylab = "Output") %>%
  ggpar(font.tickslab = 10, font.x = 10, font.y = 10)


# percentage of output lost by worker quality for 4 different task levels
quality <- seq(0.75, 1, 0.01)

tasks <- c(rep(5, length(quality)), rep(10, length(quality)), rep(50, length(quality)))

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
             xlab = "Quality",
             ylab = "% of output",
             palette = "jco") %>%
  ggpar(font.tickslab = 10, font.x = 10, font.y = 10, font.legend = 10, legend = "top", legend.title = "# of tasks") +
  theme(legend.position = c(0.2, 0.8))

ggarrange(p5, p6, ncol = 2, nrow = 1, labels = c("A", "B"), font.label = list(size = 10, face = "bold"))

ggsave(
  filename = "../figures/oring_quality.pdf",
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
