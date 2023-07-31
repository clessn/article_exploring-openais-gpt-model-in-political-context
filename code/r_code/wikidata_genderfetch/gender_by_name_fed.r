library(tidyverse)
library(tidywikidatar)

tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)

# Read MP dataset from a CSV file
data_mp_data <- read.csv("data/mp_datasets/mp_dataset.csv")

data_mp_fed <- data_mp_data %>%
  filter(level == "fed")

k <- 1
for (i in seq_along(data_mp_fed$name)) {
  print(data_mp_fed$name[i])
  loop_results <- tw_search(search = data_mp_fed$name[i]) 
  for (j in seq_along(loop_results$id)) {
    data_loop <- tw_get_qualifiers(id = loop_results$id[j], p = "P39")
    if ("Q15964890" %in% data_loop$qualifier_id) {
    print(loop_results$id[j])
    data_mp_fed$qid[k] <- data_loop$id[j]
    k <- k + 1
    }
  }
}

# Manually adding missing QIDS

# James Cumming <- Q85770627
data_mp_fed$qid[70] <- "Q85770627"

data_mp_fed$gender <- NA

for (l in seq_along(data_mp_fed$mp_id)) {
  # Get gender property for the MP
  data_loop_qid <- tw_get_property(id = data_mp_fed$qid[l], p = "P21")
  
  # Store the gender in the data frame
  data_mp_fed$gender[l] <- data_loop_qid$value
  
  print(data_mp_fed$mp_id[l])
}

# Map QIDs to human-readable gender labels
data_mp_fed$gender[data_mp_fed$gender == "Q6581097"] <- "Male"
data_mp_fed$gender[data_mp_fed$gender == "Q6581072"] <- "Female"

saveRDS(data_mp_fed, "data/outputs/data_fed_mp_43rd.rds")
