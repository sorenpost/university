library(tidyverse)
library(vroom)

##################
## READ RCA data
##################
rca_tbl <- vroom("/home/post/university/SGED07/assignments/papers/final_folder/data/scripted_data/rca/rca2010.csv")

##################
## CREATE RCA matrices
##################

## CREATE RCA MAT
rca_mat <- rca_tbl %>%
  select(iso3, hs92_4, rca_bin) %>%
  spread(key = hs92_4, value = rca_bin) %>%
  as.data.frame() %>%
  column_to_rownames(var = "iso3") %>%
  as.matrix()

## country product pairs did not exist, bc values were 0. after spreading, these are NA. I mak the make them 0.
rca_mat <- ifelse(is.na(rca_mat), 0, rca_mat)

## CREATE RCA POP MAT

rca_pop_mat <- rca_tbl %>%
  select(iso3, hs92_4, rca_pop_bin) %>%
  spread(key = hs92_4, value = rca_pop_bin) %>%
  as.data.frame() %>%
  column_to_rownames(var = "iso3") %>%
  as.matrix()

## country product pairs did not exist, bc values were 0. after spreading, these are NA. I mak the make them 0.
rca_pop_mat <- ifelse(is.na(rca_pop_mat), 0, rca_pop_mat)


##################
## FIND ECI, PCI FOR RCA MAT
##################

## create kc0 and kp0
### kc0 is diversity - how many products is in each country
kc0 <- rowSums(rca_mat)

### kp0 is ubiquity - how many countries is each product in
kp0 <- colSums(rca_mat)

## create Mcc
(rca_mat %*% t(rca_mat)[1:5, 1:5]

dim(kc0 %*% t(kp0)

Mcc

##################
## FIND ECI, PCI FOR RCA POP MAT
##################

## create kc0 and kp0
### kc0 is diversity - how many products is in each country
kc0 <- rowSums(rca_pop_mat)

### kp0 is ubiquity - how many countries is each product in
kp0 <- colSums(rca_pop_mat)
