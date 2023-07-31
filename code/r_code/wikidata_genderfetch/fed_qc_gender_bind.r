library(tidyverse)

data_fed <- readRDS("data/outputs/data_fed_mp_43rd.rds")

data_qc <- readRDS("data/outputs/data_qc_mp_42nd.rds")

data_mps_gender <- rbind(data_fed, data_qc)

saveRDS(data_mps_gender, "data/mp_datasets/full_mp_dataset.rds")
