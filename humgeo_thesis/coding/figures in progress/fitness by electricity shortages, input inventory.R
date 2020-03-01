library(tidyverse)
library(vroom)
library(foreign)
library(readxl)
library(countrycode)

# Read ES data

# Read fitness data
fitness_df <- vroom("/home/post/university/humgeo_thesis/data/downloaded/Economic_Fitness_CSV/Data.csv")

fitness_tidy <- fitness_df %>%
  select(-c(`Country Name`, `Indicator Code`), country_code = `Country Code`, metric = `Indicator Name`) %>%
  filter(metric == "Economic Fitness Metric") %>%
  gather(-c(country_code, metric), key = year, value = val)
 
# read infrastructure data
es_infrastructure <- read_excel("/home/post/university/humgeo_thesis/data/downloaded/Enterprise Survey website pulls/Enterprise Survey, National avg in manufactoring, Infrastructure.xlsx")

# add country codes (iso3)


# Identify inventory of main input variable

# Identify electricity challenge
# read infrastructure data
es_infrastructure <- read_excel("/home/post/university/humgeo_thesis/data/downloaded/Enterprise Survey website pulls/Enterprise Survey, National avg in manufactoring, Infrastructure.xlsx")
# add country codes (iso3)


# create common country list

