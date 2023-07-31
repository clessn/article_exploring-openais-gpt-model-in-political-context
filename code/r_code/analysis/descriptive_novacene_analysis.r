# Packages ----------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(gridExtra)
library(cowplot)
source("code/r_code/functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
colors <- c("CAQ" = "#00FFFF","PLQ" = "#FF0024","PQ" = "#099FFF",
            "QS" = "#FF6600","PCQ"="purple", "PLC" = "#d71920",
            "BQ" = "#33b2cc", "PCC" = "#1a4782", "NPD" = "#f37021",
            "PVC" = "darkgreen", "Ind" = "darkgrey", "Male" = "#504a4a",
            "Female" = "#ffd000", "Quebec" = "#003DA5", "ROC" = "#D80621")

prov <- readRDS("data/novacene/output_novacene_prov.rds")

themes_p <- prov %>% 
  group_by(value.Cluster) %>% 
  summarise(n = n(),
            themes = unique(value.Themes)) %>% 
  arrange(desc(n))

fed <- readRDS("data/novacene/output_novacene_fed.rds")

themes_f <- fed %>% 
  group_by(value.Cluster) %>% 
  summarise(n = n(),
            themes = unique(value.Themes))

# Label clusters ----------------------------------------------------------

prov_clusters <- c(
  "10" = "environment",
  "4" = "social_justice",
  "8" = "ecn_dev",
  "14" = "fiscal",
  "6" = "nation_qc",
  "0" = "healthcare",
  "1" = "social_progressive",
  "22" = "education",
  "2" = "innovation",
  "17" = "transparency_advocate",
  "16" = "personality",
  "3" = "gun_control",
  "18" = "parliamentary_attributes",
  "11" = "personality",
  "12" = "personality",
  "15" = "personality",
  "19" = "healthcare")

fed_clusters <- c(
  "7" = "environment",
  "9" = "social_justice",
  "15" = "ecn_dev",
  "18" = "fiscal",
  "1" = "healthcare",
  "6" = "social_justice",
  "3" = "veteran_military",
  "8" = "ecn_dev",
  "38" = "social_progressive",
  "22" = "education",
  "17" = "canadian_affairs",
  "2" = "personality",
  "0" = "ecn_dev",
  "11" = "innovation",
  "5" = "transparency_advocate",
  "13" = "parliamentary_attributes",
  "46" = "parliamentary_attributes",
  "19" = "parliamentary_attributes",
  "41" = "child_care",
  "49" = "healthcare",
  "32" = "ecn_dev")

prov$theme <- prov_clusters[as.character(prov$value.Cluster)]
fed$theme <- fed_clusters[as.character(fed$value.Cluster)]

### Manually add strings with 'gun' in fed corpus to gun_control cluster 
gun_fed <- grep("gun", fed$value)
fed$theme[gun_fed] <- "gun_control"

### Manually add strings with 'Quebec' and 'provinc' in fed corpus to nation_qc cluster 
nqc_fed <- c(grep("Quebec", fed$value), grep("Provinc", fed$value))
fed$theme[nqc_fed] <- "nation_qc"

### Manually remove strings with the following words in prov corpus
#### from nation_qc cluster

notqc_prov <- c(grep("education", prov$value),
                grep("canadian", prov$value),
                grep("Canadian", prov$value),
                grep("igital", prov$value),
                grep("iodiversity", prov$value),
                grep("Ontario", prov$value),
                grep("ilingualism", prov$value),
                grep("inguistic", prov$value),
                grep("eritage", prov$value),
                grep("oreign", prov$value),
                grep("ncreased immigration", prov$value))

### Only keep indices in the nation_qc cluster
natqc_prov <- which(prov$theme=="nation_qc")
remove_ix <- notqc_prov[notqc_prov %in% natqc_prov]

prov$theme[remove_ix] <- NA


# Bind it all -------------------------------------------------------------

Data <- prov %>% 
  select(names(fed)) %>%
  rbind(., fed) %>% 
  drop_na(theme)

### save it in data
#saveRDS(Data, "data/data.rds")

# Theme labels ------------------------------------------------------------

unique(Data$theme)
theme_labels <- c("environment" = "Environment",
                  "social_justice" = "Social\njustice",
                  "ecn_dev" = "Economic\ndevelopment",
                  "education"  = "Education",
                  "nation_qc"  = "Quebec\nnationalism",
                  "innovation" = "Innovation",
                  "fiscal" = "Fiscality",
                  "social_progressive"  = "Social\nprogressive",
                  "healthcare" = "Healthcare",
                  "transparency_advocate" = "Advocate\nof transparency",
                  "personality" = "Personality",
                  "parliamentary_attributes" = "Parliamentary\nattributes",
                  "gun_control" = "Gun control",
                  "veteran_military" = "Military",
                  "canadian_affairs" = "Canadian\naffairs",
                  "child_care" = "Child\ncare")
theme_labels

# Analysis ----------------------------------------------------------------

# most used themes
themes <- Data %>% 
  group_by(theme) %>% 
  summarise(n = n())

# most used themes by party
ByParty <- Data %>% 
  group_by(party, level, theme) %>% 
  summarise(n = n()) %>% 
  group_by(party) %>% 
  mutate(n_party = sum(n),
         prop = n/n_party) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])

# most used themes by party
ByParty2 <- Data %>% 
  group_by(party, level, theme) %>% 
  summarise(n = n()) %>% 
  group_by(party) %>% 
  mutate(n_party = sum(n),
         prop = n/n_party * 100) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])
#By level (important theme)

ByLevel <- Data %>% 
  group_by(level, theme) %>% 
  summarise(n = n()) %>% 
  group_by(level) %>% 
  mutate(n_level = sum(n),
         prop = n/n_level * 100) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])

## by fed party
ByParty %>% 
  filter(level == "fed" &
         !(party %in% c("PVC", "Ind"))) %>% 
  ggplot(aes(x = prop, y = reorder_within(theme, rank, party))) +
  geom_bar(stat = "identity",
           aes(color = party, fill = party),
           alpha = 0.7) +
  clessnverse::theme_clean_light() +
  scale_y_reordered() +
  scale_fill_manual(values = c("PLC" = "#d71920",
                               "BQ" = "#33b2cc",
                               "PCC" = "#1a4782",
                               "NPD" = "#f37021")) +
  scale_color_manual(values = c("PLC" = "#d71920",
                                "BQ" = "#33b2cc",
                                "PCC" = "#1a4782",
                                "NPD" = "#f37021")) +
  facet_wrap(~party, scales = "free_y")

ggsave("graphs/bytheme/top_themes_fed_parties.png",
       width = 10, height = 8)


## by prov party
ByParty %>% 
  filter(level == "prov") %>% 
  ggplot(aes(x = prop, y = reorder_within(theme, rank, party))) +
  geom_bar(stat = "identity",
           aes(color = party, fill = party),
           alpha = 0.7) +
  clessnverse::theme_clean_light() +
  scale_y_reordered() +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  facet_wrap(~party, scales = "free_y")

ggsave("graphs/bytheme/top_themes_prov_parties.png",
       width = 10, height = 8)

# by theme

# top parties by theme
ByTheme <- Data %>% 
  group_by(party, level, theme) %>% 
  summarise(n = n()) %>% 
  group_by(party) %>% 
  mutate(n_party = sum(n),
         prop = n/n_party) %>%
  group_by(theme) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number())


for (i in 1:length(unique(ByParty$theme))){
  themei <- unique(ByParty$theme)[i]
  ByTheme %>% 
    filter(theme == themei &
           party != "Ind") %>% 
    ggplot(aes(x = prop, y = reorder_within(party, rank, theme))) +
    geom_bar(stat = "identity",
             aes(color = party, fill = party),
             alpha = 0.7) +
    clessnverse::theme_clean_light() +
    scale_y_reordered() +
    scale_fill_manual(values = c("PLC" = "#d71920",
                                 "BQ" = "#33b2cc",
                                 "PCC" = "#1a4782",
                                 "NPD" = "#f37021",
                                 "PVC" = "darkgreen",
                                 colors)) +
    scale_color_manual(values = c("PLC" = "#d71920",
                                  "BQ" = "#33b2cc",
                                  "PCC" = "#1a4782",
                                  "NPD" = "#f37021",
                                  "PVC" = "darkgreen",
                                  colors)) +
    facet_wrap(~level, scales = "free_y") +
    labs(title = themei)
#  ggsave(paste0("graphs/bytheme/", themei, ".png"),
#                width = 10, height = 8)
}


# proportion by MP before agregating  


# Spider charts -----------------------------------------------------------

for (i in 1:length(names(colors))){
  p <- names(colors)[i]
  spider_graph(ByParty, p)
  ggsave(paste0("graphs/byparty/", p, ".png"),
         width = 12, height = 8)
}


# By party on only one graph  ---------------------------------------------

## Instead of spider graphs for parties, we will put all parties on same plot
### we need to do one plot by level (fed and prov) and then combine with gridextra

### 1. federal level
pfed <-
  ByParty %>% 
  # remove independant candidates
  filter(party != "Ind" &
           level == "fed") %>% 
  ggplot(aes(x = prop*100, y = reorder(label, prop))) +
  ## points
  geom_point(aes(color = party),
             shape = 19,
             alpha = 0.4,
             size = 8) +
  scale_color_manual(values = colors,
                     labels = c("BQ" = "BQ",
                                "NPD" = "NDP",
                                "PCC" = "CPC",
                                "PLC" = "LPC",
                                "PVC" = "GPC")) +
  clessnverse::theme_clean_light() +
  ylab("") +
  xlab("") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))  
### 1. prov level
pprov <- ByParty %>% 
  # remove independant candidates
  filter(level == "prov") %>% 
  ggplot(aes(x = prop*100, y = reorder(label, prop))) +
  ## points
  geom_point(aes(color = party),
             shape = 19,
             alpha = 0.4,
             size = 8) +
  scale_color_manual(values = colors,
                     labels = c("PQ" = "PQ",
                                "QS" = "QS",
                                "CAQ" = "CAQ",
                                "PLQ" = "QLP")) +
  clessnverse::theme_clean_light() +
  ylab("") +
  xlab("") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

# combine federal graph and prov graph
plot_grid(pfed, pprov,
          nrow = 1) +
  draw_text(text = "Proportion of characteristics of party (%)",
            y = 0.07,
            x = 0.55,
            fontface = "plain",
            size = 13,
            color = "#2A2A2A")

ggsave(filename = "graphs/byparty/by_party.png",
       width = 10, height = 10)  



###
### -------------------------- Spider Graph Gender ------------------------- ###
###

# By gender ---------------------------------------------------------------

data_mp_gender <- readRDS("data/mp_datasets/full_mp_dataset.rds") %>%
  select(mp_id, gender)

Data <- left_join(Data, data_mp_gender, by = "mp_id")

by_gender <- Data %>% 
  group_by(gender, level, theme) %>% 
  summarise(n = n()) %>% 
  group_by(gender, level) %>% 
  mutate(n_gender = sum(n),
         prop = n/n_gender) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])


by_gender2 <- Data %>% 
  group_by(gender, level, theme) %>% 
  summarise(n = n()) %>% 
  group_by(gender, level) %>% 
  mutate(n_gender = sum(n),
         prop = n/n_gender * 100) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])

## by fed gender
by_gender %>% 
  filter(level == "fed") %>% 
  ggplot(aes(x = prop, y = reorder_within(theme, rank, gender))) +
  geom_bar(stat = "identity",
           aes(color = gender, fill = gender),
           alpha = 0.7) +
  clessnverse::theme_clean_light() +
  scale_y_reordered() +
  scale_fill_manual(values = c("Male" = "#504a4a",
                               "Female" = "#ffd102")) +
  scale_color_manual(values = c("Male" = "#504a4a",
                                "Female" = "#ffd102")) +
  facet_wrap(~gender, scales = "free_y")

ggsave("graphs/bygender/top_themes_fed_parties.png",
       width = 10, height = 8)

by_gender %>% 
  filter(level == "prov") %>% 
  ggplot(aes(x = prop, y = reorder_within(theme, rank, gender))) +
  geom_bar(stat = "identity",
           aes(color = gender, fill = gender),
           alpha = 0.7) +
  clessnverse::theme_clean_light() +
  scale_y_reordered() +
  scale_fill_manual(values = c("Male" = "#504a4a",
                               "Female" = "#ffd102")) +
  scale_color_manual(values = c("Male" = "#504a4a",
                                "Female" = "#ffd102")) +
  facet_wrap(~gender, scales = "free_y")

ggsave("graphs/bygender/top_themes_prov_parties.png",
       width = 10, height = 8)


orders <- 1:length(theme_labels)
names(orders) <- theme_labels

by_gender %>%
  ungroup() %>% 
  filter(level == "fed") %>%
  mutate(order = orders[label],
         label = as.character(label)) %>%
  arrange(order, gender) %>% 
  ggplot(aes(x = reorder(label, order), y = prop)) +
  theme_minimal_grid() +
  geom_polygon(aes(group = gender, fill = gender),
               size = 1,
               alpha = 0.4,
               show.legend = TRUE) +
  coord_radar() +
  ylab("Proportion of MPs<br>with characteristic<br>") +
  scale_color_manual(values = colors, name = "") +
  scale_fill_manual( values = colors, name = "") +
  scale_y_continuous(limits = c(0, 0.30)) +
  theme(axis.title.x = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = ggtext::element_markdown(size = 17.5),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16)) +
  geom_text(x = 0, y = 0.05, label = "5%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.24, y = 0.10, label = "10%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.34, y = 0.15, label = "15%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.40, y = 0.20, label = "20%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.42, y = 0.25, label = "25%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.44, y = 0.30, label = "30%", size = 6.75, color = "#2a2a2a", hjust = 0.65)

ggsave("graphs/bygender/by_gender_fed.png",
       width = 12, height = 10)


by_gender %>%
  ungroup() %>% 
  filter(level == "prov") %>%
  mutate(order = orders[label],
         label = as.character(label)) %>%
  arrange(order, gender) %>% 
  ggplot(aes(x = reorder(label, order), y = prop)) +
  theme_minimal_grid() +
  theme(plot.background = element_rect(fill = "white")) +
  geom_polygon(aes(group = gender, fill = gender),
               size = 1,
               alpha = 0.4,
               show.legend = TRUE) +
  coord_radar() +
  ylab("Proportion of MPs<br>with characteristic<br>") +
  scale_color_manual(values = colors, name = "") +
  scale_fill_manual( values = colors, name = "") +
  scale_y_continuous(limits = c(0, 0.30)) +
  theme(axis.title.x = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = ggtext::element_markdown(size = 17.5),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16)) +
  geom_text(x = 0, y = 0.05, label = "5%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.24, y = 0.10, label = "10%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.34, y = 0.15, label = "15%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.40, y = 0.20, label = "20%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.42, y = 0.25, label = "25%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.44, y = 0.30, label = "30%", size = 6.75, color = "#2a2a2a", hjust = 0.65) 
  
ggsave("graphs/bygender/by_gender_qc.png",
       width = 12, height = 10)



# By canadian region -----------------------------------------------------

canadian_regions <- c("West", "West", "Atlantic", "West",
                      "Atlantic", "Atlantic", "Territory",
                      "Ontario", "Quebec", "West", "Atlantic",
                      "Territory", "Territory")

names(canadian_regions) <- sort(unique(Data$province))
canadian_regions


by_region <- Data %>%
  filter(level == "fed") %>%
  mutate(region = canadian_regions[province],
         qc_roc = ifelse(region=="Quebec", "Quebec", "ROC")) %>% 
  group_by(qc_roc, theme) %>% 
  summarise(n = n()) %>% 
  group_by(qc_roc) %>% 
  mutate(n_qc_roc = sum(n),
         prop = n/n_qc_roc) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])

by_region2 <- Data %>%
  filter(level == "fed") %>%
  mutate(region = canadian_regions[province],
         qc_roc = ifelse(region=="Quebec", "Quebec", "ROC")) %>% 
  group_by(qc_roc, theme) %>% 
  summarise(n = n()) %>% 
  group_by(qc_roc) %>% 
  mutate(n_qc_roc = sum(n),
         prop = n/n_qc_roc * 100) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])
## graph
by_region %>%
  ungroup() %>% 
  mutate(order = orders[label],
         label = as.character(label)) %>%
  arrange(order, qc_roc) %>% 
  ggplot(aes(x = reorder(label, order), y = prop)) +
  theme_minimal_grid() +
  theme(plot.background = element_rect(fill = "white")) +
  geom_polygon(aes(group = qc_roc, fill = qc_roc),
               size = 1,
               alpha = 0.4,
               show.legend = TRUE) +
  coord_radar() +
  ylab("Proportion of MPs<br>with characteristic<br>") +
  scale_color_manual(values = colors, name = "") +
  scale_fill_manual( values = colors, name = "") +
  scale_y_continuous(limits = c(0, 0.30)) +
  theme(axis.title.x = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = ggtext::element_markdown(size = 17.5),
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size = 16)) +
  geom_text(x = 0, y = 0.05, label = "5%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.24, y = 0.10, label = "10%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.34, y = 0.15, label = "15%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.40, y = 0.20, label = "20%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.42, y = 0.25, label = "25%", size = 6.75, color = "#2a2a2a", hjust = 0.65) +
  geom_text(x = 0.44, y = 0.30, label = "30%", size = 6.75, color = "#2a2a2a", hjust = 0.65) 

ggsave("graphs/byregion/by_qc_roc.png",
       width = 12, height = 10)




## With provincial level ---------------------------------------------------

by_region <- Data %>%
  mutate(region = canadian_regions[province],
         qc_roc = ifelse(region=="Quebec", "Quebec", "ROC"),
         qc_roc = ifelse(is.na(region), "Quebec", qc_roc)) %>% 
  group_by(qc_roc, theme) %>% 
  summarise(n = n()) %>% 
  group_by(qc_roc) %>% 
  mutate(n_qc_roc = sum(n),
         prop = n/n_qc_roc) %>% 
  arrange(., prop) %>% 
  mutate(rank = row_number(),
         label = theme_labels[theme])

## graph
by_region %>%
  ungroup() %>% 
  mutate(order = orders[label],
         label = as.character(label)) %>%
  arrange(order, qc_roc) %>% 
  ggplot(aes(x = reorder(label, order), y = prop)) +
  theme_minimal_grid() +
  theme(plot.background = element_rect(fill = "white")) +
  geom_polygon(aes(group = qc_roc, fill = qc_roc),
               size = 1,
               alpha = 0.4,
               show.legend = TRUE) +
  coord_radar() +
  ylab("Proportion of MPs\nwith characteristic") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 0.3)) +
  theme(axis.title.x = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  geom_text(x = 0, y = 0.05, label = "5%", size = 5, color = "black") +
  geom_text(x = 0.24, y = 0.10, label = "10%", size = 5, color = "black") +
  geom_text(x = 0.34, y = 0.15, label = "15%", size = 5, color = "black") +
  geom_text(x = 0.40, y = 0.20, label = "20%", size = 5, color = "black") +
  geom_text(x = 0.42, y = 0.25, label = "25%", size = 5, color = "black") +
  geom_text(x = 0.44, y = 0.30, label = "30%", size = 5, color = "black") +
  labs(caption = "Theme labeling is derived from Novacene AI clustering algorithm.") 

ggsave("graphs/byregion/by_qc_roc_with_prov_level.png",
       width = 12, height = 10)

>>>>>>> 5060eacb25f5b4b55b60d477600d49cdda38a2f8