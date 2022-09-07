## Script juts to look at Aulacoseira

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")
library("vegan");packageVersion("vegan")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Import combined EMP & AEU data files 
load("RData/df_phyto.RData")

df_Aul <- df_phyto %>% filter(Genus == "Aulacoseira")

ggplot(df_Aul, aes(x = Year, y = GALD)) +
  geom_jitter(width = 0.1)

ggplot(df_Aul, aes(x = Year, y = BV.Avg)) +
  geom_jitter(width = 0.1)



