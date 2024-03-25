# Scoring data for NCAA Tournament 2024

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(here)

# Set directory for storing plots
output <- here("ncaa-bracket-pool","2024","plots")

# Set visual theme in ggplot
theme_set(theme_bw())

# Import bracket data ----------------------------------------------------------
df_hoops <- read_excel(here("ncaa-bracket-pool","2024","report-m.xlsx"))

df_points_possible <- read_csv(here("ncaa-bracket-pool","2024","points_possible_by_round.csv"))

df_hoops <- pivot_longer(df_hoops, 
                         names_to = "Round_ID", 
                         values_to = "Pick", 
                         cols = `E-R1-G1`:last_col())

df_names <- as_tibble(sort(unique(df_hoops$Pick)))

# Check for consistency in school names ----------------------------------------
# Fix typos and inconsistencies

df_hoops <- df_hoops %>% 
  mutate(Pick = str_replace_all(Pick,"Long Beach St.","Long Beach")) %>% 
  mutate(Pick = str_replace_all(Pick,"Tenessee","Tennessee")) %>% 
  mutate(Pick = str_replace_all(Pick,"Montanta St.","Montana St.")) %>% 
  mutate(Pick = str_replace_all(Pick,"Florida St.","Florida")) %>% 
  mutate(Pick = str_replace_all(Pick,"San Diego St.","San Diego St")) %>% 
  mutate(Pick = str_replace_all(Pick,"Mississippi St.","Mississippi")) %>% 
  mutate(Pick = str_replace_all(Pick,"Mississippi","Mississippi St.")) %>% 
  mutate(Pick = str_replace_all(Pick,"N Carolina","North Carolina")) %>% 
  mutate(Pick = str_replace_all(Pick,"Utah St.","Utah")) %>% 
  mutate(Pick = str_replace_all(Pick,"Utah","Utah State"))
  
df_names <- as_tibble(sort(unique(df_hoops$Pick))) #61 teams

# Read in CSV with results------------------------------------------------------
df_scores <- read_excel(here("ncaa-bracket-pool","2024","ncaa-results-m-2024.xlsx"))

df_hoops <- left_join(df_hoops,df_scores,by="Round_ID")

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

df_plot <- pivot_wider(df_tallies, 
                       names_from = "Result", 
                       values_from = "Points",
                       values_fill = 0)

df_plot <- df_plot %>%
  mutate(`Points Possible` = Correct + Points_Left)

df_plot <- df_plot %>% select(Name:Correct,`Points Possible`)

df_plot <- pivot_longer(df_plot, 
                        names_to = "Category", 
                        values_to = "Points",
                        cols = Correct:`Points Possible`)

df_plot$Category <- gsub("Correct","Points Scored",df_plot$Category)

# Plot Total Points and 
p_totals <- ggplot(df_plot, aes(y = reorder(Name, Points, sum), 
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
       title = "NCAA Bracket Challenge 2023 - EMRR - First Round, Day 2") 

ggsave(path = output,
       filename = "NCAA_bracket_scores_R1D2.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")
