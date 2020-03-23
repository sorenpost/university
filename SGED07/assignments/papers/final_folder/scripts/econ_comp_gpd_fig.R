library(tidyverse)
library(janitor)
library(ggpubr)

# read and prep nat resource rents (from WB Estimates based on sources and methods described in "The Changing Wealth of Nations: Measuring Sustainable Development in the New Millennium" ( World Bank, 2011 ).)
nat_rents_tb <- read_csv("/home/post/university/SGED07/assignments/papers/final_folder/data/nat_res_rents.csv", skip = 4) %>%
  select(-c(`Indicator Name`, `Indicator Code`, X65)) %>%
  gather(-c(`Country Name`, `Country Code`), key = year, value = perc_rent_of_gdp) %>%
  mutate(year = as.numeric(year)) %>%
  clean_names(case = "snake")

# read and prep gdp cap levels, gdp/cap ppp 2011 intl dollars
gdp_cap_tb <- read_csv("/home/post/university/SGED07/assignments/papers/final_folder/data/API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_867594/gdp_cap_ppp_2011_int_doll.csv", skip = 4) %>%
    select(-c(`Indicator Name`, `Indicator Code`, X65)) %>%
  gather(-c(`Country Name`, `Country Code`), key = year, value = gdp_cap) %>%
  mutate(year = as.numeric(year)) %>%
  clean_names(case = "snake")

# read and prep econ comp levels, from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FXTAQMC&version=DRAFT
eci_tb <- read_delim("/home/post/university/SGED07/assignments/papers/final_folder/data/eci_rankings.tab", delim = "\t") %>%
  select(country_code = code, year, hs_eci, hs_eci_rank)

# join tabels
col_tb <- eci_tb %>%
  left_join(nat_rents_tb) %>%
  left_join(gdp_cap_tb) %>%
  mutate(
    rents_10 = as.factor(ifelse(perc_rent_of_gdp >= 10, "above 10%", "below 10%")),
    rents_20 = as.factor(ifelse(perc_rent_of_gdp >= 20, 1, 0)),
    rents_30 = as.factor(ifelse(perc_rent_of_gdp >= 30, 1, 0)),
  ) %>%
  mutate(gdp_cap_ln = log(gdp_cap))

col2010 <- col_tb %>%
  filter(year == 2010)

p1 <- ggscatter(col2010,
          y = "gdp_cap_ln",
          x = "hs_eci",
          ylab = "ln(GDP/cap)",
          xlab = "Economic Complexity (ECI)",
          color = "rents_10",
          shape = "rents_10",
          add = "reg.line",
          add.params = list(size = 0.5),
          legend.title = "Resource rents as % of GDP:",
          font.legend = 12,
          font.tickslab = 10,
          font.x = 12,
          font.y = 12) +
  scale_shape_manual(values = c(20, 4)) +
  scale_color_manual(values = c("#253494", "#bd0026")) +
  stat_cor(aes(color = rents_10), label.x = 0.7, label.y = c(8, 8.3)) +
  stat_cor(label.x)

p1

cor(col2010$hs_eci, col2010$gdp_cap_ln, method = "pearson", na.rm = TRUE)

?cor

  stat_cor(aes(color = cyl), label.x = 3)