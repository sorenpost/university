###################
## PREPARE BACI DATA
###################

## Data and docs from: CEPII BACI database: http://www.cepii.fr/DATA_DOWNLOAD/baci/doc/DescriptionBACI.html
## Read trade data
##

library(vroom)
library(tidyverse)

bilateral_trade <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/BACI-full/BASCI_HS_92/BACI_HS92_Y2010_V202001.csv") %>%
  rename(
    year = t,
    hs92_6 = k,
    country = i,
    export_value = v
  )

## raw data is bilateral, turn into total exports of product by one country

exports <- bilateral_trade %>%
  group_by(year, country, hs92_6) %>%
  summarise(
    export_value = sum(export_value)
  )


country_codes <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/BACI-full/BASCI_HS_92/country_codes_V202001.csv") %>%
  select(country = country_code, iso_3digit_alpha)

exports <- left_join(exports, country_codes)

## exports is in 6 digit. turn it into 4 digit
exports <- exports %>%
  mutate(
  hs92_4 = str_trunc(hs92_6, width = 4, side = "right", ellipsis = "")
  )

## aggregate to 4-digit level. group by year, country, hs92_4 and sum export value
exports <- exports %>%
  group_by(year, iso_3digit_alpha, hs92_4) %>%
  summarise(
    export_value = (sum(export_value) * 1000) # bc original data is reported in thousands of usd
  ) %>%
  rename(iso3 = iso_3digit_alpha) %>%
  ungroup()

## save unfiltered exports for later comp
exports_unfiltered <- exports

## Add export filters:
## remove code 9999 ("commodities not specifiec"), and the two codes that fall to 0, 2527, 1403.
exports <- exports %>%
  filter(!(hs92_4 %in% c("9999", "2527", "1403")))

## remove iraq, quality issues.
exports <- exports %>%
  filter(iso3 != "IRQ")

## remove countries with smaller total export that 1 billion
low_exports <- exports %>%
  group_by(year, iso3) %>%
  summarize(
    total_country_exp = sum(export_value)
  ) %>%
  filter(total_country_exp < 1000000000)

exports <- exports %>%
  filter(!iso3 %in% low_exports$iso3)

## HS 4 data is now ready.

##########################
## PREPARE POPULATION DATA
#########################
##
## Data from WDI, World Bank: https://data.worldbank.org/indicator/SP.POP.TOTL
pop <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/population/API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv", skip = 4) %>%
  select(
    country_name = `Country Name`, iso3 = `Country Code`, pop = `2010`
  ) %>%
  mutate(
    year = 2010
    )

## join pop data to exports.  remove country codes without population data. these are typically small islands or city states: saint helena, falklands, montserrat etc.
exports <- left_join(exports, pop) %>%
  filter(!is.na(pop))

## also remove countries w. pop below 1 mio
exports <- exports %>%
  filter(pop >= 1000000)

#########################
## CALCULATE RCApop
########################

## formula for RCApop is (country c's export of product p divided by population of country c) / (worlds total export of product p divided by worlds total population)
## define variables
total_exports_by_product <- exports %>%
  group_by(year, hs92_4) %>%
  summarise(
    global_exp_in_prod = sum(export_value)
  ) %>%
  ungroup()

total_pop <- sum(exports$pop)

exports <- exports %>%
  left_join(total_exports_by_product) %>%
  mutate(rca_pop = (export_value / pop) / (global_exp_in_prod / total_pop)) %>%
  mutate(rca_pop_bin = ifelse(rca_pop >= 1, 1, 0))


## for comparison, I also calculate the regular rca. rca is defined as (country c's export of product p divided by total exports of country c) / (world total export of product p divided by world total exports)

## country wide total export
country_total_export <- exports %>%
  group_by(iso3, year) %>%
  summarise(
    country_total_export = sum(export_value)
  )

global_total_export <- sum(exports$export_value)

exports <- exports %>%
  left_join(country_total_export) %>%
  mutate(
    rca = (export_value / country_total_export) / (global_exp_in_prod / global_total_export)
  ) %>%
  mutate(
  rca_bin = ifelse(rca >= 1, 1, 0))

## Tidy it up
rca_tidy <- exports %>%
  select(
    year,
    iso3,
    hs92_4,
    rca_pop,
    rca_pop_bin,
    rca,
    rca_bin)

## compare:
old_nr_c <- exports_unfiltered %>%
  pull(iso3) %>%
  unique() %>%
  length()

new_nr_c <- exports %>%
  pull(iso3) %>%
  unique() %>%
  length()

old_nr_c - new_nr_c
new_nr_c

old_exp_sum <- sum(exports_unfiltered$export_value)
new_exp_sum <- sum(exports$export_value)

(old_exp_sum - new_exp_sum) / old_exp_sum

## write file
write_csv(rca_tidy, "/home/post/university/SGED07/assignments/papers/final_folder/data/scripted_data/rca/rca2010.csv")
