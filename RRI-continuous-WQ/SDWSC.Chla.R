## Calculate Average and Std Dev 
## for Water Temp at RRI

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("timetk");packageVersion("timetk")
library("cder");packageVersion("cder")

setwd("C:/Users/tflynn.WATER/OneDrive - California Department of Water Resources/projects/SDWSC DO/R")

rm(list=ls()) #cleans workspace
theme_set(theme_bw())

WQ <- read_csv("RRI data/chl 1m.csv",
               skip = 4)
#col_types = "dcdddcdcdccccccccc"
#)

## Remove unneeded columns
WQ$STATION_ID <- NULL
WQ$`...9` <- NULL
WQ$READING_TYPE <- NULL
WQ$RESULT_ID <- NULL
WQ$`STATION NAME` <- NULL
WQ$`Row #` <- NULL

WQ <- WQ %>% rename("Chla.1m" = "VALUE")
WQ <- WQ %>% rename("DateTime" = "DATE")
WQ <- WQ %>% rename("QAQC" = "QAQC Flag")

# Remove seconds data
WQ$DateTime <- round(WQ$DateTime, units = "mins")

WQ.G <- WQ %>% filter(WQ$QAQC != "X")



##Import data from 3 meters

WQ.3m <- read_csv("RRI data/chl 3m.csv",
                  skip = 4)
#col_types = "dcdddcdcdccccccccc"
#)

## Remove unneed columns
WQ.3m$STATION_ID <- NULL
WQ.3m$`...9` <- NULL
WQ.3m$READING_TYPE <- NULL
WQ.3m$RESULT_ID <- NULL
WQ.3m$`STATION NAME` <- NULL
WQ.3m$`Row #` <- NULL

WQ.3m <- WQ.3m %>% rename("Chla.3m" = "VALUE")
WQ.3m <- WQ.3m %>% rename("DateTime" = "DATE")
WQ.3m <- WQ.3m %>% rename("QAQC" = "QAQC Flag")

## Round to nearest minute to enable joining data together
WQ.3m$DateTime <- round(WQ.3m$DateTime, units = "mins")

WQ.3m.G <- WQ.3m %>% filter(WQ.3m$QAQC != "X")


WQ.G$QAQC <- NULL
WQ.3m.G$QAQC <- NULL

WQ.all <- full_join(WQ.G, WQ.3m.G)

##Import data from 6 meters

WQ.6m <- read_csv("RRI data/chl 6m.csv",
                  skip = 4)
#col_types = "dcdddcdcdccccccccc"
#)

## Remove unneed columns
WQ.6m$STATION_ID <- NULL
WQ.6m$`...9` <- NULL
WQ.6m$READING_TYPE <- NULL
WQ.6m$RESULT_ID <- NULL
WQ.6m$`STATION NAME` <- NULL
WQ.6m$`Row #` <- NULL

WQ.6m <- WQ.6m %>% rename("Chla.6m" = "VALUE")
WQ.6m <- WQ.6m %>% rename("DateTime" = "DATE")
WQ.6m <- WQ.6m %>% rename("QAQC" = "QAQC Flag")

## Round to nearest minute to enable joining data together
WQ.6m$DateTime <- round(WQ.6m$DateTime, units = "mins")

WQ.6m.G <- WQ.6m %>% filter(WQ.6m$QAQC != "X")

WQ.6m.G$QAQC <- NULL

WQ.all <- full_join(WQ.all, WQ.6m.G)

WQ.all.long <- pivot_longer(WQ.all, names_to = "Depth", cols = 2:4)

## Convert sample dates into months (numbered)
## and append onto existing data frame in column "month"
WQ.all.long <- WQ.all.long %>%
  mutate(Month = month(DateTime))

WQ.all.long <- WQ.all.long %>%
  mutate(month2 = as.Date(paste0("2019-",
                                 WQ.all.long$Month,
                                 "-01),%Y-%m-%d")))

## Convert month numbers into text months (e.g., Jan, Feb, Mar)
## and append as a column named month3
WQ.all.long <- WQ.all.long %>%
  mutate(Month = month(WQ.all.long$month2, label=TRUE))

## Order month 3 in calendar order rather than (default) alphabetical
WQ.all.long$Month = factor(WQ.all.long$Month, levels = month.abb)
WQ.all.long$month2 <- NULL

## Add column of just the year for highlighting yearly data
WQ.all.long <- WQ.all.long %>%
  mutate(Year = year(WQ.all.long$DateTime))

## Add column of just the year for highlighting yearly data
## Add Julian date for plotting
WQ.all.long <- WQ.all.long %>%
  mutate(Julian = yday(WQ.all.long$DateTime))

## Use years as character not number
WQ.all.long <- WQ.all.long %>%
  mutate(Year = as.character(WQ.all.long$Year))

WQ.all.long$Julian <- as.character(WQ.all.long$Julian)

WQ.all.long <- WQ.all.long %>% rename("Chla" = "value")

## Rename Depths
#WQ.all.long$Depth <- gsub("T.1m","1m",WQ.all.long$Depth)
#WQ.all.long$Depth <- gsub("T.3m","3m",WQ.all.long$Depth)
#WQ.all.long$Depth <- gsub("T.6m","6m",WQ.all.long$Depth)

## Plot Monthly Temps plus error bars
Chla.monthly.means <- WQ.all.long %>% 
  group_by(Depth, Month) %>%
  summarize(Mean = mean(Chla, na.rm = T), SD = sd(Chla, na.rm = T)) %>%
  ungroup()

mon.mn.Chla.plot <- ggplot(Chla.monthly.means, aes(x = Month, y = Mean)) +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.minor = element_blank()) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,  position=position_dodge(.9)) +
  geom_point(position=position_dodge(.9), stat="identity") +
  labs(x = "Month",
       y = "Chlorophyll a (ug/L)",
       title = "Monthly Mean Chlorophyll a at RRI")

mon.mn.Chla.plot + facet_wrap(Depth ~ ., ncol = 1)

ggsave(path="plots",
       filename = "mon.mn.Chla.plot.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=6,
       width=4, 
       dpi="print")

## Plot boxplots by day
plot <- ggplot(WQ.all.long, aes(x = Julian, y = value)) +
  geom_boxplot(data = subset(WQ.all.long, Month == "Sep")) 
  #geom_smooth(data = subset(WQ.all.long, Month == "Sep"), method = "loess", se = F)

plot + 
  facet_wrap(Depth ~ ., ncol = 1)

## Plot Daily Chl a plus error bars
CDEC.RRI <- cdec_query("RRI", sensors = c("28"),  
                       start.date = as.POSIXct("2021-08-27"), 
                       end.date = as.POSIXct("2021-09-12"))

## Remove outliers
CDEC.RRI <- CDEC.RRI %>% filter(Value < 20)

## Remove midnight-only value on 9/12
CDEC.RRI <- CDEC.RRI %>% filter(DateTime < as.POSIXct("2021-09-12"))

## Plot Daily Mean Value
daily.means <- CDEC.RRI %>%
  mutate(date = floor_date(DateTime, unit = "day")) %>%
  rename("Chla" = "Value") %>%
  group_by(date) %>%
  summarize(Mean = mean(Chla, na.rm = T), SD = sd(Chla, na.rm = T)) %>%
  ungroup()

plot.daily.mean.Chla <- ggplot(daily.means, aes(x = date, y = Mean)) +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.minor = element_blank()) +
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d") +
  geom_point(position=position_dodge(.9), stat="identity", size = 4) +
  geom_hline(aes(yintercept=3.3), color = "blue") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1.1)) +
  #geom_text(y = 3.5, x = as.POSIXct("2021-09-06"), label = "Mean Chl a for September, 2008 - 2020") +
  labs(x = NULL,
       y = "Chlorophyll a (μg/L)") 

plot.daily.mean.Chla + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD))

ggsave(path="plots",
       filename = "daily.mn.Chla.Sept.plot.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3,
       width=5.5, 
       dpi="print")
