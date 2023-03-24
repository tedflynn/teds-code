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
df_points_possible <- read_csv("points_possible_by_round.csv")

df_hoops <- pivot_longer(df_hoops, 
                         names_to = "Name", 
                         values_to = "Pick", 
                         cols = `Andrew Tran`:last_col())

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

df_hoops <- left_join(df_hoops, df_points_possible)

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
  mutate(Result = case_when(Pick == Winner ~ "Correct",
                            Pick %in% Loser ~ "Incorrect",
                            #is.na(Winner) ~ "Not Played",
                            TRUE ~ "Points_Left"))

df_tallies <- df_hoops %>%
  group_by(Name, Result) %>%
  summarize(Points = sum(Points_Possible)) %>%
  ungroup()

# Sum up all to check if they all add up to 192 (the total points possible)
df_tallies %>% 
  group_by(Name) %>% 
  summarize(Total = sum(Points)) # all add up to 192

df_plot <- pivot_wider(df_tallies, names_from = "Result", values_from = "Points")

df_plot <- df_plot %>%
  mutate(`Points Possible` = Correct + Points_Left)

df_plot <- df_plot %>% select(Name:Correct,`Points Possible`)

df_plot <- pivot_longer(df_plot, 
                           names_to = "Category", 
                           values_to = "Points",
                           cols = Correct:`Points Possible`)

df_plot$Category <- gsub("Correct","Points Scored",df_plot$Category)

# Plot Total Points and 
p_totals <- ggplot(df_plot, aes(y = reorder(Name, Points), 
                                x = Points, 
                                fill = Category)) +
  geom_bar(width = 0.7,
           position = "dodge",
           stat = "summary", 
           fun = "sum") +
  #geom_point(data = subset(df_plot, Category == "Points_Possible"), size = 4) +
  #scale_x_continuous(breaks = c(3,6,9,12)) +
  scale_fill_manual(values = c("gold3","darkblue"))

p_totals +
  labs(x = "Total Points Scored",
       y = NULL,
       title = "NCAA Bracket Challenge 2023 - EMRR - Sweet 16, Day 1") 

ggsave(path = output,
       filename = "NCAA_bracket_scores_S16D1.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Plot Points by Round
df_score_by_round <- df_hoops %>%
  group_by(Name, Round, Result) %>%
  summarize(Points = sum(Points_Possible)) %>%
  ungroup()

df_score_by_round <- df_score_by_round %>%
  filter(Result == "Correct")

p_score_by_round <- ggplot(df_score_by_round, aes(x = Points,
                                                  y = reorder(Name, Points),
                                                  fill = Round)) +
  geom_bar(width = 0.7,
           position = "stack",
           stat = "summary", 
           fun = "sum") +
  scale_fill_brewer(palette = "Set1")
                             
p_score_by_round
