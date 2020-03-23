library(tidyverse)
library(vroom)
library(janitor) 


ref_year <- 2010

####################
## READ ATLAS RCA AND PCI DATA
###################

atlas <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/economic-complexity/harvard_country_hsproduct4digit_year.tab", delim = "\t") %>%
  select(
    year,
    iso3 = location_code,
    hs92 = hs_product_code,
    export_value,
    export_rca,
    normalized_pci,
    pci
  )

###################
## READ GDP CAP DATA
###################

# read and prep gdp cap levels, gdp/cap ppp 2011 intl dollars
gdp_cap <- read_csv("/home/post/university/SGED07/assignments/papers/final_folder/data/API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_867594/gdp_cap_ppp_2011_int_doll.csv", skip = 4) %>%
  select(-c(`Indicator Name`, `Indicator Code`)) %>%
  gather(-c(`Country Name`, `Country Code`), key = year, value = gdp_cap) %>%
  mutate(year = as.numeric(year)) %>%
  clean_names(case = "snake") %>%
  rename(iso3 = country_code)


##########################
## PREPARE POPULATION DATA
#########################
##
## Data from WDI, World Bank: https://data.worldbank.org/indicator/SP.POP.TOTL
pop <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/population/API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv", skip = 4) %>%
  select(
    iso3 = `Country Code`, pop = `2010`
  ) %>%
  mutate(
    year = 2010
    )


####################
## LIMIT TO REF YEAR AND JOIN
###################

atlas2010 <- filter(atlas, year == ref_year)
gdp_cap2010 <- filter(gdp_cap, year == ref_year)
rm(ls = "atlas")

joined <- atlas2010 %>%
  left_join(gdp_cap2010) %>%
  left_join(pop) %>%
  mutate(
    rca05_dummy = ifelse(export_rca >= 0.5, 1, 0),
    rca1_dummy = ifelse(export_rca >= 1, 1, 0)
  )

###################
## FILTERS
####################


## filter countries that A) does not have pop data, and B) have less than 1 mio inhabitants
joined <- joined %>%
  filter(!is.na(pop)) %>%
  filter(pop >= 1000000) 

## Remove "9999", "financial", "ict", "travel", "transport", "unspecificed"
joined <- joined %>%
  filter(
    !(hs92 %in% c("9999", "XXXX", "unspecified", "travel", "transport", "ict", "financial"))
  )

## filter countries that exported for less that 1 bia in 2010. 
low_exports <- joined %>% 
  group_by(iso3) %>%
  summarize(total_exp = sum(export_value)) %>%
  filter(total_exp < 1000000000) %>%
  pull(iso3)

joined <- joined %>% 
  filter(!(iso3 %in% low_exports))

## filter products that are very low ubuiquity

low_ubiq <- joined %>%
  group_by(hs92) %>%
  summarize(
    ubiq05 = sum(rca05_dummy, na.rm = TRUE),
    ubiq1 = sum(rca1_dummy, na.rm = TRUE)
  ) %>% 
  filter(ubiq05 < 10 | ubiq1 < 10)

joined <- joined %>%
  filter(!(hs92 %in% low_ubiq$hs92))

#################
## Write file
################

joined_final <- joined

write_csv(joined_final, "/home/post/university/SGED07/assignments/papers/final_folder/data/scripted_data/rca/atlas_modified_for_least_most.csv")

