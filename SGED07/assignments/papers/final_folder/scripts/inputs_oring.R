library(tidyverse)
library(ggpubr)

tb <- tibble(n = 1:20) %>%
  mutate(
    o99 = n*(0.99^n),
    o95 = n*(0.95^n),
    o90 = n*(0.90^n)
  ) %>%
  gather(-n, key = risk, val = output)

ggline(
  tb,
  x = "n",
  y = "output",
  linetype = "risk",
  plot_type = "l",
)


tb <- tibble(z = seq(0.75, 1, 0.01)) %>%
  mutate(
    n2 = 2*(z^2),
    n5 = 5*(z^5),
    n10 = 10*(z^10)
  ) %>%
  gather(-z, key = n, val = output)

ggline(
  tb,
  x = "z",
  y = "output",
  linetype = "n",
  plot_type = "l",
)
