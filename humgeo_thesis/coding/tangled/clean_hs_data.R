library(here)
library(tidyverse)
library(vroom)

hs_path <- here("data/downloaded/trade/country_hsproduct4digit_year.tab")
hs_raw <- vroom(hs_path, delim = "\t")

hs <- hs_raw %>%
	select(
        time = year,
        region = location_code,
        unit = hs_product_code,
        intensity = export_value
    )

ref_year <- 2010
ref_export <- 1000000000
ref_pop <- 1250000
ref_reliable <- c("AFG", "TCD", "IRQ")

available_set <- filter(hs, time == ref_year) %>%
    pull(region) %>%
    unique()

export_set <- hs %>%
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
        region = str_to_upper(iso3c)
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

reliable_set <- hs %>%
	filter(!(region %in% ref_reliable)) %>%
	pull(region) %>%
	unique()

hs_filtered <- hs %>%
	filter(region %in% export_set) %>%
	filter(region %in% pop_set) %>%
	filter(region %in% available_set) %>%
	filter(region %in% reliable_set)

write_csv(
  hs_filtered,
  here("data/prepared/hs92_filtered_ti.csv")
)

product_prop_limit <- 0.80
region_prop_limit <- 0.95
product_export_min <- 10000000

product_summary <- hs_filtered %>%
    group_by(time, unit) %>%
    summarise(
        regions_with_0_export = sum(intensity == 0),
        total_regions = n(),
        proportion_0 = regions_with_0_export / total_regions
    )

unit_year_exclusions <- product_summary %>%
    filter(proportion_0 >= product_prop_limit)

region_summary <- hs_filtered %>%
    group_by(time, region) %>%
    summarise(
        units_with_0_export = sum(intensity == 0),
        total_units = n(),
        proportion_0 = units_with_0_export / total_units
    )

region_year_exclusions <- region_summary %>%
    filter(proportion_0 >= region_prop_limit)

global_product_export_vals <- hs_filtered %>%
    group_by(time, unit) %>%
    summarise(
        total_export_val = sum(intensity)
    )

global_product_exclusions <- global_product_export_vals %>%
    filter(total_export_val < product_export_min)

hs_filtered_time_dep <- anti_join(hs_filtered, unit_year_exclusions) %>%
    anti_join(region_year_exclusions) %>%
    anti_join(global_product_exclusions)

output_path <- here("data/prepared/hs92_filtered_ti_and_td")
write_csv(hs_filtered_time_dep, output_path)
