# Scoring data for NCAA Tournament

library(tidyverse)
library(RColorBrewer)
library(janitor)

setwd("./NCAA-pool-2023")
getwd()

# Clean workspace
rm(list=ls()) 

# Set directory for storing plots
output <- "plots"

# Set visual theme in ggplot
theme_set(theme_bw())

# Import bracket data ----------------------------------------------------------
df_hoops <- read_csv("ncaa_bracket_pool_data_2023.csv")

df_hoops <- pivot_longer(df_hoops, 
                         names_to = "Name", 
                         values_to = "Pick", 
                         cols = `Andrew Tran`:`Melinda Baerwald`)

# Check for consistency in school names ----------------------------------------
# Fix typos and inconsistencies
df_hoops$Pick <- gsub("Miama","Miami",df_hoops$Pick)
df_hoops$Pick <- gsub("Maimi","Miami",df_hoops$Pick)
df_hoops$Pick <- gsub("Northwetern","Northwestern",df_hoops$Pick)
df_hoops$Pick <- gsub("Uconn","UConn",df_hoops$Pick)
df_hoops$Pick <- gsub("Prude","Purdue",df_hoops$Pick)
df_hoops$Pick <- gsub("UVA","Virginia",df_hoops$Pick)
df_hoops$Pick <- gsub("Connecticut","UConn",df_hoops$Pick)
df_hoops$Pick <- gsub("FAU","Florida",df_hoops$Pick)

df_names <- as_tibble(sort(unique(df_hoops$Pick))) #61 teams
# only 3 teams weren't picked: Grand Canyon, N. Kentucky, Fair. Dickinson

# Read in CSV with scores ------------------------------------------------------
df_scores <- read_csv("ncaa-scores-2023.csv")

df_hoops <- left_join(df_hoops,df_scores)

# Set order for rounds
round.order <- c("First Round",
                 "Round of 32",
                 "Sweet 16",
                 "Elite Eight",
                 "Final Four",
                 "Title Game")

df_hoops$Round <- factor(df_hoops$Round, level = round.order)

# Check winners and score brackets
df_hoops <- df_hoops %>%
  mutate(Result = case_when(Pick == Winner ~ 1, TRUE ~ 0)) %>%
  mutate(Score = case_when(Round == "First Round" ~ Result * 1, 
                           Round == "Round of 32" ~ Result * 2,
                           Round == "Sweet 16" ~ Result * 4,
                           Round == "Elite Eight" ~ Result * 8,
                           Round == "Final Four" ~ Result * 16,
                           Round == "Title Game" ~ Result * 32))

df_table <- df_hoops %>%
  group_by(Name, Round) %>%
  summarize(Total = sum(Score)) %>%
  ungroup()

df_total <- df_hoops %>%
  group_by(Name) %>%
  summarize(Total = sum(Score)) %>%
  ungroup()

p_total <- ggplot(df_table, aes(x = reorder(Name, -Total), y = Total, fill = Round)) +
  geom_col(width = 0.7) +
  scale_fill_brewer(palette = "Set1")

p_total +
  labs(x = "Player Name",
       y = "Total Points",
       title = "NCAA Tournament Bracket Challenge - EMRR - 2023") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(path = output,
       filename = "NCAA_bracket_scores.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")
