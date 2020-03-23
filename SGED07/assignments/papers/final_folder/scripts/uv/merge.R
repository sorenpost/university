library(tidyverse)
library(vroom)


######## 
## READ BILATERAL TRADE DATA FOR 2015
#######

# see  http://www.cepii.fr/DATA_DOWNLOAD/baci/doc/DescriptionBACI.html for description and docs

trade_flows <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/BACI-full/BACI_HS96_V202001/BACI_HS96_Y2015_V202001.csv",
                     col_types = c(
                       i = "c",
                       j = "c"
                     )
)  %>%
  mutate(
    i = str_pad(.$i, width = 3, side = "left", pad = "0"),
    j = str_pad(.$j, width = 3, side = "left", pad = "0"),
  ) 

# read zeros file to catch if any observation that is not included is not included bc its is a 0.



trade_flows_zeros <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/BACI-full/zeros_HS96_V202001.csv")

trade_flows_zeros %>% filter(t == 2015)


trade_flows <- trade_flows %>%
  mutate(
    i = str_pad(trade_flows$i, width = 3, side = "left", pad = "0"),
    j = str_pad(trade_flows$j, width = 3, side = "left", pad = "0"),
  ) %>%
  rename(
    year = t,
    export_iso = i,
    import_iso = j, 
    hs96code = k, # 6 digit code
    export_val = v, # thousands us dollars
    quantity = q # metric tons
  ) %>%
  mutate(is_in_tf = 1)
    

#######
## READ TRADE UNIT VALUES
#######

# for docs see http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=2
unit_vals <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/trade-unit-value/tuv_96_x_2015.csv") %>%
  rename(
    export_iso = r, 
    import_iso = p,
    hs96code = hs6_96,
    year = yr
    ) %>%
  mutate(
    is_in_tu = 1
  )
  

joined <- unit_vals %>%
  full_join(trade_flows)

is_in_both <- joined %>% 
  filter(is_in_tf == 1 & is_in_tu ==1) %>%
  tally()

in_tu_not_tf <- joined %>%
  filter(is_in_tu == 1 & is.na(is_in_tf)) %>%
  tally()

in_tf_not_tu <- joined %>%
  filter(is_in_tf == 1 & is.na(is_in_tu)) %>%
  tally()


in_tf_not_tu
