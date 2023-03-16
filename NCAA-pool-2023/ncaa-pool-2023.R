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

# Score brackets
df_hoops <- df_hoops %>%
  mutate(Points.1 = case_when(Round == "First Round" & Pick == Winner ~ 1,
                            TRUE ~ 0)) %>%
  mutate(Points.2 = case_when(Round == "Round of 32" & Pick == Winner ~ 2,
                            TRUE ~ 0)) %>%
  mutate(Points.3 = case_when(Round == "Sweet Sixteen" & Pick == Winner ~ 4,
                            TRUE ~ 0)) %>%
  mutate(Points.4 = case_when(Round == "Elite Eight" & Pick == Winner ~ 8,
                            TRUE ~ 0)) %>%
  mutate(Points.5 = case_when(Round == "Final Four" & Pick == Winner ~ 16,
                            TRUE ~ 0)) %>%
  mutate(Points.6 = case_when(Round == "Title Game" & Pick == Winner ~ 32,
                            TRUE ~ 0))




