library(tidyverse)

data_10char <- read_csv("data/outputs/key_characteristics_tweaked.csv")
problems(data_10char)

data_mp <- read_csv("data/mp_datasets/mp_dataset.csv")
problems(data_mp)

data_gender <- readRDS("data/mp_datasets/full_mp_dataset.rds") %>%
    select(c(mp_id, gender))

data_10char <- data_10char %>%
    select(-c(name, name2))

data_analysis <- merge(data_mp, data_10char, by = "mp_id")
data_analysis <- merge(data_analysis, data_gender, by = "mp_id")

data_analysis <- data_analysis %>%
    pivot_longer(starts_with("c"), names_to = "char", values_to = "value")


saveRDS(data_10char, "data/outputs/key_chars.rds")
saveRDS(data_analysis, "data/outputs/data_analysis.rds")
