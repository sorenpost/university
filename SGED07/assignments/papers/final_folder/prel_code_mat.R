# read PCI data

# read 

rca2015_df <- rca2015_mat %>%
  as.data.frame() %>%
  rownames_to_column(var = "region") %>%
  gather(-region, key = unit, value = rca) %>%
  as_tibble()

complexity_order <- fit2015_df %>% 
  filter(metric == "complexity") %>%
  filter(iteration == max(iteration)) %>%
  select(unit = name, value) %>% 
  arrange(value)

fitness_order <- fit2015_df %>% 
  filter(metric == "fitness") %>%
  filter(iteration == max(iteration)) %>%
  select(region = name, value) %>% 
  arrange(value)

rca2015_df <- rca2015_df %>% mutate(
  unit = factor(unit, levels = complexity_order$unit),
  region = factor(region, levels = fitness_order$region))

ggplot(rca2015_df, aes(x = unit, y = region, fill = rca)) +
  geom_raster() +
  scale_fill_gradient(high = "#009ccc", low =  "#e6f9ff",
                      space = "Lab", na.value = "grey50", guide = FALSE,
                      aesthetics = "fill") +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank())