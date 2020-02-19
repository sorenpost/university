library(here)
library(tidyverse)
library(vroom)

sitc_path <- here("data/downloaded/trade/country_sitcproduct4digit_year.tab")
sitc_raw <- vroom(sitc_path, delim = "\t")

sitc <- sitc_raw %>%
	select(
        time = year,
        region = location_code,
        unit = sitc_product_code,
        intensity = export_value
    )

ref_year <- 2010
ref_export <- 1000000000
ref_pop <- 1250000
ref_reliable <- c("AFG", "TCD", "IRQ")

available_set <- filter(sitc, time == ref_year) %>%
    pull(region) %>%
    unique()

export_set <- sitc %>%
	filter(time == ref_year) %>%
	group_by(region) %>%
	summarise(
        total_export = sum(intensity)
    ) %>%
	filter(total_export >= ref_export) %>%
	pull(region) %>%
    unique()

library(WDI)

pop_raw <- WDI(country = "all", indicator = "SP.POP.TOTL", extra = TRUE) %>%
	as_tibble()

pop_data <- pop_raw %>%
	mutate_if(is.factor, as.character) %>%
	mutate(
        region = str_to_lower(iso3c)
    ) %>%
	select(
        time = year,
        region,
        pop = SP.POP.TOTL
    )

pop_set <- pop_data %>%
	filter(time == ref_year & pop >= ref_pop) %>%
	pull(region) %>%
    unique()

reliable_set <- sitc %>%
	filter(!(region %in% ref_reliable)) %>%
	pull(region) %>%
	unique()

sitc_filtered <- sitc %>%
	filter(region %in% export_set) %>%
	filter(region %in% pop_set) %>%
	filter(region %in% available_set) %>%
	filter(region %in% reliable_set)

product_prop_limit <- 0.80
region_prop_limit <- 0.95
product_export_min <- 10000000

    group_by(time, unit) %>%
    summarise(
        regions_with_0_export = sum(intensity == 0),
        total_regions = n(),
        proportion_0 = regions_with_0_export / total_regions
    )

unit_year_exclusions <- product_summary %>%
    filter(proportion_0 >= product_prop_limit)

region_summary <- sitc_tidy %>%
    group_by(time, region) %>%
    summarise(
        units_with_0_export = sum(intensity == 0),
        total_units = n(),
        proportion_0 = units_with_0_export / total_units
    )

region_year_exclusions <- region_summary %>%
    filter(proportion_0 >= region_prop_limit)

global_product_export_vals <- sitc_tidy %>%
    group_by(time, unit) %>%
    summarise(
        total_export_val = sum(intensity)
    )

global_product_exclusions <- global_product_export_vals %>%
    filter(total_export_val < product_export_min)

sitc_filtered2 <- anti_join(sitc_tidy, unit_year_exclusions) %>%
    anti_join(region_year_exclusions) %>%
    anti_join(global_product_exclusions)

output_path <- "~/sorensfolder/sbpdata/data/sitc2_4digit_both_filters.csv"
write_csv(sitc_filtered2, output_path)
