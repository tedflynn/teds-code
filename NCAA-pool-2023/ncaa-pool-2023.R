# Scoring data for NCAA Tournament

library(tidyverse)
library(RColorBrewer)

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
df_hoops$Pick <- gsub("Florida","FAU",df_hoops$Pick)

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

# Check winners and score brackets ---------------------------------------------
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

p_total <- ggplot(df_table, aes(y = reorder(Name, Total), x = Total, fill = Round)) +
  geom_col(width = 0.7) +
  #scale_x_continuous(breaks = c(3,6,9,12)) +
  scale_fill_brewer(palette = "Set1")

p_total +
  labs(x = "Total Points Scored",
       y = NULL,
       title = "NCAA Tournament Bracket Challenge - EMRR - 2023") 
  
ggsave(path = output,
       filename = "NCAA_bracket_scores.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Calculate how many points remaining ------------------------------------------
df_total_points <- df_hoops %>%
  select(Round_ID, Round) %>%
  mutate(Multiplier = case_when(is.na(Round_ID) ~ 1, TRUE ~ 1)) %>%
  mutate(Score = case_when(Round == "First Round" ~ Multiplier * 1, 
                           Round == "Round of 32" ~ Multiplier * 2,
                           Round == "Sweet 16" ~ Multiplier * 4,
                           Round == "Elite Eight" ~ Multiplier * 8,
                           Round == "Final Four" ~ Multiplier * 16,
                           Round == "Title Game" ~ Multiplier * 32))

df_total_points <- unique(df_total_points)

sum(df_total_points$Score) # Total points possible = 192

df_hoops <- df_hoops %>%
  mutate(Result.L = case_when(Pick %in% df_hoops$Loser ~ 1, TRUE ~ 0)) %>%
  mutate(Score.L = case_when(Round == "First Round" ~ Result.L * 1, 
                           Round == "Round of 32" ~ Result.L * 2,
                           Round == "Sweet 16" ~ Result.L * 4,
                           Round == "Elite Eight" ~ Result.L * 8,
                           Round == "Final Four" ~ Result.L * 16,
                           Round == "Title Game" ~ Result.L * 32))

df_hoops <- df_hoops %>%
  relocate(Result.L, .after = Result)

df_points <- df_hoops %>%
  group_by(Name) %>%
  summarize(across(Score:Score.L, ~ sum(.x, na.rm = TRUE))) %>%
  mutate(Points_Possible = 192 - Score.L) %>%
  rename("Points_Scored" = "Score") %>%
  select(Name:Points_Scored,Points_Possible) %>%
  ungroup()

df_points <- pivot_longer(df_points, 
                          names_to = "Category",
                          values_to = "Points",
                          cols = Points_Scored:Points_Possible)


df_points_left <- df_hoops %>%
  group_by(Name) %>%
  summarize(Total = sum(Score.L)) %>%
  ungroup()

# Calculate how many points each person has left
df_points_left <- df_points_left %>%
  mutate(Points_Possible = 192 - Total)

# Plot total points so far plus potential points left to be scored
p_total_pot <- ggplot(df_points_left, aes(y = reorder(Name, -Total), x = Points_Possible)) +
  geom_col(width = 0.7) +
#  scale_x_continuous(breaks = c(3,6,9,12)) +
  scale_fill_brewer(palette = "Set1")

p_total_pot +
  labs(x = "Points Possible",
       y = NULL,
       title = "NCAA Tournament Bracket Challenge - EMRR - 2023") 

ggsave(path = output,
       filename = "NCAA_bracket_scores.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

