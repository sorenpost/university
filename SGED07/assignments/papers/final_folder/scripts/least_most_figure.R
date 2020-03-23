library(tidyverse)
library(vroom)
library(janitor) 
library(ggpubr)

##############
## READ DATA
############

tbl <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/scripted_data/rca/atlas_modified_for_least_most.csv") %>%
  select(
    -c(normalized_pci, country_name, pop)
  )

##############
## CALC AVG log GDP/CAP FOR PRODUCERS, 5 RICHEST, 5 POOREST
#############

## first, create a table of the 5 lowest gdp exporters

low_rank_five_and_below <- tbl %>%
  mutate(
    exporter_gdp = ifelse(rca1_dummy == 1, gdp_cap, NA),
#    ln_exporter_gdp = exporter_gdp
    ln_exporter_gdp = log(exporter_gdp)
  ) %>%
  group_by(hs92) %>%
  mutate(
    rank = row_number(exporter_gdp) # lower is lower in rank, 1 = lowest gdp
  ) %>%
  filter(
    rank < 6
  )
 
## group by hs 92, take mean exporter_gdp
mean_five_lowest <- low_rank_five_and_below %>%
  group_by(hs92) %>%
  summarise(
    pci = mean(pci), # doesn't matter, all values for a product are the same 
    mean_ln_gdp_lowest = mean(ln_exporter_gdp)
  )

## second, create a table of the 5 highest gdp exporters

high_rank_five_and_below <- tbl %>%
  mutate(
    exporter_gdp = ifelse(rca1_dummy == 1, gdp_cap, NA),
#    ln_exporter_gdp = exporter_gdp
    ln_exporter_gdp = log(exporter_gdp)
  ) %>%
  group_by(hs92) %>%
  mutate(
    rank = row_number(desc(exporter_gdp)) # higher is lower in rank, 1 = highest gdp
  ) %>%
  filter(
    rank < 6
  )
 

## group by hs 92, take mean exporter_gdp
mean_five_highest <- high_rank_five_and_below %>%
  group_by(hs92) %>%
  summarise(
    pci = mean(pci), # doesn't matter, all values for a product are the same 
    mean_ln_gdp_highest = mean(ln_exporter_gdp)
  )

##################
## JOIN TABLES
#################

mean_tidy <- left_join(mean_five_highest, mean_five_lowest) %>%
  rename(mean_5_high = mean_ln_gdp_highest, mean_5_low = mean_ln_gdp_lowest) %>%
  gather(-c(hs92, pci), key = group, value = mean_ln_gdp) %>%
  mutate(
    group = ifelse(group == "mean_5_high", "richest five exporters", "poorest five exporters")
  )

p1 <- ggscatter(
  mean_tidy,
  x = "pci",
  xlab = "Product Complexity (PCI)",
  y = "mean_ln_gdp",
  ylab = "ln(GDP/cap)",
  color = "group",
  shape = "group",
  add = "reg.line",
  add.params = list(size = 0.5),
  font.legend = 12,
  font.tickslab = 10,
  legend.title = "Average ln(GPD/cap) of:",
  font.x = 12,
  font.y = 12
) + 
  scale_shape_manual(values = c(4, 20)) +
  scale_color_manual(values = c("#253494", "#bd0026"))
p1


