library(tidyverse)
library(tidywikidatar)

tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)

# Read MP dataset from a CSV file
data_mp_data <- read.csv("data/mp_datasets/mp_dataset.csv")

data_mp_qc <- data_mp_data %>%
  filter(level == "prov")

k <- 1
for (i in seq_along(data_mp_qc$name)) {
  print(data_mp_qc$name[i])
  loop_results <- tw_search(search = data_mp_qc$name[i]) 
  for (j in seq_along(loop_results$id)) {
    data_loop <- tw_get_qualifiers(id = loop_results$id[j], p = "P39")
    if ("Q3305338" %in% data_loop$qualifier_id) {
    print(loop_results$id[j])
    data_mp_qc$qid[k] <- data_loop$id[j]
    k <- k + 1
    }
  }
}

# Manually adding missing QIDS
#  Mathieu Lévesque (Q59783153) [21]
data_mp_qc$qid[21] <- "Q59783153"

#  François Tremblay (Q56877074) [33]
data_mp_qc$qid[33] <- "Q56877074"

#  Alexandre Leduc (Q56876714) [41]
data_mp_qc$qid[41] <- "Q56876714" 

#  Sol Zanetti (Q16677378) [47]
data_mp_qc$qid[47] <- "Q16677378"

#  Harold LeBel (Q16244162) [95]
data_mp_qc$qid[95] <- "Q16244162"

#  Marwah Rizqy (Q56875892) [107]
data_mp_qc$qid[107] <- "Q56875892"


data_mp_qc$gender <- NA


for (l in seq_along(data_mp_qc$mp_id)) {
  # Get gender property for the MP
  data_loop_qid <- tw_get_property(id = data_mp_qc$qid[l], p = "P21")
  
  # Store the gender in the data frame
  data_mp_qc$gender[l] <- data_loop_qid$value
  
  print(l)
}

# Map QIDs to human-readable gender labels
data_mp_qc$gender[data_mp_qc$gender == "Q6581097"] <- "Male"
data_mp_qc$gender[data_mp_qc$gender == "Q6581072"] <- "Female"

saveRDS(data_mp_qc, "data/outputs/data_qc_mp_42nd.rds")
