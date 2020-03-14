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

p2 <- ggline(output_tb,
       numeric.x.axis = TRUE,
       x = "quality",
       y = "output",
       plot_type = "l",
       xlab = "Quality",
       ylab = "Output")


# percentage of output lost by worker quality for 4 different task levels
quality <- seq(0.75, 1, 0.01)

tasks <- c(rep(5, length(quality)), rep(10, length(quality)), rep(50, length(quality)), rep(100, length(quality)))

 scaled_output_tb <- tibble(qual = rep(quality, 4), tasks) %>%
 mutate(output = qual^(tasks)*tasks) %>%
   mutate(rescaled_output = output/tasks) %>%
   mutate(tasks = as.factor(tasks))

p3 <- ggline(scaled_output_tb,
             numeric.x.axis = TRUE,
             #group = "tasks",
             x = "qual",
             y = "rescaled_output",
             plot_type = "l",
             xlab = "Quality",
             ylab = "Output",
             color = "tasks",
             palette = "jco")


figure <- ggarrange(p2, p3,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)

figure
