Data <- read.csv("data/outputs/data_analysis.csv", encoding = "UTF-8")

# Create a ready-to-be-analyzed file for the provincial government
Data %>% 
  filter(level == "prov") %>% 
  write.csv(., "data/novacene/input_novacene_prov.csv", fileEncoding = "UTF-8")

# Create a ready-to-be-analyzed file for the federal government
Data %>% 
  filter(level == "fed") %>% 
  write.csv(., "data/novacene/input_novacene_fed.csv", fileEncoding = "UTF-8")
