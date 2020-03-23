library(tidyverse)

## input-output table
library(readxl)

io_tab <- read_excel("/home/post/university/SGED07/assignments/papers/final_folder/data/iotable/CxC_TR_2007_2012_PRO_DET.xlsx",
           sheet = "2012",
           skip = 4) %>%
  select(-c(`Commodity Description`))

io_tidy <- io_tab %>% 
  rename(
    user = Code
  ) %>%
  gather(-user, key = supplier, value = val)

io_mat <- io_tab %>%
  column_to_rownames(var = "Code") %>%
  as.matrix()
 
# First i'm interested in the share of each commodity in the production of each commodity. Commoditiy that buys are columns. Thus i divide each cell by its column sum.

colsums_io_mat <- colSums(io_mat)
share_mat <- t(t(io_mat) / colsums_io_mat)
# to see that R takes divides a matrix by a vector col_wise, see:
mat <- matrix(1:16, ncol = 4, byrow = TRUE)
mat / c(1, 0, 0, 0)
mat
# i there need to turn the matrix. i turn it back after
t(t(mat) / c(1, 0, 0, 0))

# or see:
t(io_mat[5, 2]) /  colsums_io_mat[2] == share_mat[5, 2]
 
# i now create the squared shares and sum
sq_share_mat <- share_mat^2

hh_index <- colSums(sq_share_mat)

hh_index_df <- hh_index %>%
  as.data.frame() %>%
  rownames_to_column(var = "industry") %>%
  as_tibble()


mat <- ifelse(share_mat > 0.1, 1, 0)


library(igraph)


nrow(mat)
colnames(mat)

g <- graph_from_adjacency_matrix(mat, mode = "directed")


