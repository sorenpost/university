library("tidyverse")
library("vroom")
library("janitor")
library(countrycode)

#######################################################
## Prepare unit values
#######################################################
# read exporter data (exports are the reporters)
unit_val <- vroom("/home/post/university/humgeo_thesis/data/downloaded/trade/tuv_96_x_2015.csv") %>%
  rename(
    year = yr,
    unit_value = uv
  ) %>%
  filter(year == 2015)

reporter_codes <- vroom("/home/post/university/humgeo_thesis/data/downloaded/trade/reporter_coverage96_x0017.csv") %>%
  select(exp_country = name, r)

partner_codes <- vroom("/home/post/university/humgeo_thesis/data/downloaded/trade/partner96_x0017.csv") %>%
  select(imp_country = name, p)

# join names to reporters and get iso3c codes
unit_val <- unit_val %>%
  left_join(reporter_codes) %>%
  left_join(partner_codes) %>%
  mutate(
    exp_iso3c = countrycode(.$exp_country, origin = "country.name", destination = "iso3c"),
    imp_iso3c = countrycode(.$imp_country, origin = "country.name", destination = "iso3c")
    ) 

# remove all but first 4 digits, group by r, p, 
unit_val <- unit_val %>%
  mutate(hs4_96 = str_extract(hs6_96, "^.{4}")) %>%
  filter(year == 2015)


# unit_val only shows unit values, not intensity. this means its impossible to add a country avg.  %>%


# read bil. trade data 
hs96_bil <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/hs61996/year_origin_dest_hs96_2015.csv") %>%
  select(-c(import_val)) 

# add export values to 
hs96_bil <- hs96_bil %>%
  rename(hs4_96 = hs96) %>%
  mutate(
    exp_iso3c = str_to_upper(origin),
    imp_iso3c = str_to_upper(dest)
  )

#######################################################
## 
#######################################################

# 2 add country codes to unit value DB

# 3 prep export + rca data, source: https://oec.world/en/resources/data/ HS6 rev. 1996 (1998 - 2017), Product Trade by Year and Country (6 digit depth)
hs96 <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/hs61996/year_origin_hs96_6.tsv", delim = "\t") %>%
  mutate(
    export_val = as.numeric(export_val),
    export_rca = as.numeric(export_rca)
  ) %>%
  filter(year == 2015) %>%
  select(-c(import_val, import_rca))

# 4 read + prep pci data
pci96 <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/pci_hs96_rankings_2011_2017.csv") %>%
  clean_names(case = "snake") %>%
  mutate(hs6_96 = str_extract(hs96_id, "^.{6}")) %>%
  filter(year == 2015)
 
# 5 read + prep eci data
eci <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/eci_country_rankings_2013_2017(1).csv") %>% 
  clean_names(case = "snake") %>%
  filter(year == 2015)


# -------
unit_val %>%
  filter(country == "Afghanistan") %>%
  group_by(hs6_96) %>%
  summarise(
    s_d = sd(unit_value),
    mean = mean(unit_value),
    coef_var = sd(unit_value)/mean(unit_value)
  )
