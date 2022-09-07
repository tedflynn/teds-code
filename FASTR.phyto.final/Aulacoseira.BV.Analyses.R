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

# Import AEU data files (comment out when finished)
phyto_files <- dir(path = "data/csv", pattern = "\\.csv", full.names = T)
phyto.all <- map_dfr(phyto_files, ~read_csv(.x))

# Clean up column names
phyto <- phyto.all %>%
  clean_names(case = "big_camel")

phyto.Aul <- phyto %>% filter(Genus == "Aulacoseira")

phyto.Aul$SampleDate <- mdy(phyto.Aul$SampleDate)

phyto.Aul <- phyto.Aul %>%
  mutate(Year = year(SampleDate))

phyto.Aul$Year <- as.factor(phyto.Aul$Year)

ggplot(phyto.Aul, aes(x = Year, y = Gald)) +
  geom_jitter(width = 0.1)

ggplot(phyto.Aul, aes(x = Year, y = Biovolume1)) +
  geom_jitter(width = 0.1)

write_csv(phyto.Aul, file = "Aul.csv")

