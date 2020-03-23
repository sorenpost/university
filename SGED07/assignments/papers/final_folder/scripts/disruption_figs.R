library(tidyverse)
library(ggpubr)
library(readxl)
library(janitor)
library(countrycode)

## read file from enterprise survey database.
es_tb <- read_csv("/home/post/university/SGED07/assignments/papers/final_folder/data/es_infra.csv") %>%
  clean_names(case = "snake") %>%
  group_by(economy) %>%
  filter(year > 2007 & year < 2016) %>%
  filter(year == max(year)) %>%
  ungroup()

## add iso-3 codes
es_tb <- es_tb %>%
  mutate(
    iso3c = countrycode(es_tb$economy, origin = "country.name", destination = 'iso3c')
  ) %>%
  select(-c(subgroup, top_subgroup_level, subgroup_level))

es_crime_tb <- read_csv("/home/post/university/SGED07/assignments/papers/final_folder/data/es_crime.csv") %>%
  clean_names(case = "snake") %>%
  group_by(economy) %>%
  filter(year < 2016) %>%
  filter(year == max(year)) %>%
  ungroup()

es_tb <- es_tb %>% left_join(es_crime_tb %>%
                               select(economy, year, products_shipped_to_supply_domestic_markets_that_were_lost_due_to_theft_percent_of_product_value))

## read eci and fitness data
eci_tb <- read_csv("/home/post/university/Contagious_disruptions_complexity_trap_economic_development-2.0.4/empirical_data/ECI_country_rankings.csv") %>%
  clean_names(case = "snake") %>%
  rename(iso3c = abbrv)

## read fitness values
fit_tb <- read_csv("/home/post/university/humgeo_thesis/data/downloaded/Economic_Fitness_CSV/Data.csv")  %>%
  filter(`Indicator Name` == "Economic Fitness Metric") %>%
  select(-c(`Country Name`, `Indicator Name`, `Indicator Code`, X26)) %>%
  rename(iso3c = `Country Code`) %>%
  gather(-iso3c, key = year, value = fitness) %>%
  mutate(year = as.numeric(year))

## add gdp/cap 2010
gdp_cap_tb <- read_csv("/home/post/university/SGED07/assignments/papers/final_folder/data/API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_867594/gdp_cap_ppp_2011_int_doll.csv",
                       skip = 4) %>%
  select(-c(`Country Name`, `Indicator Name`, `Indicator Code`, X65)) %>%
  rename(iso3c = `Country Code`) %>%
  gather(-iso3c, key = year, value = gdp_cap) %>%
  mutate(year = as.numeric(year))

## add eci, gdp, fit to es_tb
tidy_tb <- es_tb %>%
  left_join(eci_tb) %>%
  left_join(gdp_cap_tb) %>%
  left_join(fit_tb)

## make plots
tidy_tb <- tidy_tb %>%
  mutate(
    if_there_were_outages_average_losses_due_to_electrical_outages_percent_of_annual_sales = as.numeric(if_there_were_outages_average_losses_due_to_electrical_outages_percent_of_annual_sales),
    monthly_water_short = as.numeric(number_of_water_insufficiencies_in_a_typical_month),
    gdp_cap_ln = log(gdp_cap)
  ) %>%
  mutate(
    no_eci = as.factor(ifelse(is.na(eci_value), 1, 0))
  )

#### PLOT IT ####

sz <- 2

## % products break, spoil in shipment
p1 <- ggscatter(
  tidy_tb,
  x = "gdp_cap_ln",
  y = "proportion_of_products_lost_to_breakage_or_spoilage_during_shipping_to_domestic_markets_percent",
  size = sz,
  color = "eci_value",
  add = "reg.line",
  ylab = " ",
  xlab = "ln(GDP/cap)",
  add.params = list(color = "black") # Customize reg. line
) %>% ggpar(legend = "right",
            font.legend = 10,
            legend.title = "ECI",
            font.tickslab = 10,
            font.x = 10,
            font.y = 8,
            font.title = 10,
            title = "% products spoiled, broken in shipment") +
  gradient_color(c("white", "blue"))
  
  

## % sales lostdue to electrical shortages
p2 <- ggscatter(
  tidy_tb,
  size = sz,
  x = "gdp_cap_ln",
  y = "if_there_were_outages_average_losses_due_to_electrical_outages_percent_of_annual_sales",
  color = "eci_value",
  add = "reg.line",
  ylab = " ",
  xlab = "ln(GDP/cap)",
  add.params = list(color = "black") # Customize reg. line
) %>% ggpar(font.tickslab = 10, font.x = 10, font.y = 8, font.title = 10, title = "% yearly sales lost to electricity shortages") +
  gradient_color(c("white", "blue"))

##  water shortages per month
p3 <- ggscatter(
  tidy_tb,
  x = "gdp_cap_ln",
  y = "monthly_water_short",
  color = "eci_value",
  size = sz,
  add = "reg.line",
  ylab = " ",
  xlab = "ln(GDP/cap)",
  add.params = list(color = "black") # Customize reg. line
) %>% ggpar( font.tickslab = 10, font.x = 10, font.y = 8, font.title = 10, title = "% product value stolen during shipment") +
  gradient_color(c("white", "blue"))

## products_shipped_to_supply_domestic_markets_that_were_lost_due_to_theft_percent_of_product_value
p4 <- ggscatter(
  tidy_tb,
  x = "gdp_cap_ln",
  y = "products_shipped_to_supply_domestic_markets_that_were_lost_due_to_theft_percent_of_product_value",
  color = "eci_value",
  size = sz,
  add = "reg.line",
  ylab = " ",
  xlab = "ln(GDP/cap)",
  add.params = list(color = "black"), # Customize reg. line
  na.rm = FALSE
) %>% ggpar( font.tickslab = 10, font.x = 10, font.y = 8, font.title = 10, title = "# of monthly water shortages")+
  gradient_color(c("white", "blue"))
  




## join plots
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right") 

ggsave("../figures/disruption_figs.png")
