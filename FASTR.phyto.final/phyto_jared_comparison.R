## Recreate Jared's 2016 data from raw phyto datasheets
## 5/27/2022
## TMF

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")

# Set working directory
setwd("C:/Users/tflynn.WATER/Documents/R/FASTR.phyto.redo/")

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

## Load density data 
load("phyto_raw.RData")

## Subset to 2016 data only
phyto <- phyto %>%
  filter(Year == 2016)

## Remove Unit.Density Data
phyto$Unit.Density <- NULL

## Pivot to long format
## Summarize biovolume by genus
phyto.l <- pivot_longer(phyto, cols = 8, 
                        names_to = "Type", 
                        values_to = "Conc")

## Summarize by genus
phyto.gen <- phyto.l %>% 
  group_by(Type, Year, Month, DateTime, StationCode, Genus) %>%
  summarize(Conc = sum(Conc, na.rm = T)) %>%
  ungroup

## Summarize by algal group
phyto.grp <- phyto.l %>% 
  group_by(Type, Year, Month, DateTime, StationCode, Group) %>%
  summarize(Conc = sum(Conc, na.rm = T)) %>%
  ungroup

## Add zeros for missing taxonomic groups
phyto.grp.w <- pivot_wider(phyto.grp, names_from = Group, values_from = Conc)

phyto.grp.w <- phyto.grp.w %>% replace(is.na(.), 0)

phyto.grp <- pivot_longer(phyto.grp.w, cols = 6:12,
                     names_to = "Group",
                     values_to = "Conc")

##### Add Cat Pien's Code for Merging Flow Designations #####
#FlowDesignation <- read_csv("FlowDatesDesignations.csv")
## Update with 45 days limit on either end
FlowDesignation <- read_csv("FlowDatesDesignations_45days.csv")

#FlowDesignation$Year <- ordered(FlowDesignation$Year)
#WQ$Year <- ordered(WQ$Year)
FlowDesignation$PreFlowStart <- mdy(FlowDesignation$PreFlowStart)
FlowDesignation$PreFlowEnd <- mdy(FlowDesignation$PreFlowEnd)
FlowDesignation$PostFlowStart <- mdy(FlowDesignation$PostFlowStart)
FlowDesignation$PostFlowEnd <- mdy(FlowDesignation$PostFlowEnd)

# Merge data from FlowDesignation Table (Water Year Type, Flow days and type)
# Filter only Pre-During-Post Flow Action Data. 
phyto.grp <- inner_join(phyto.grp,FlowDesignation, by = "Year")   
phyto.grp <- phyto.grp %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime > PreFlowEnd & DateTime < PostFlowStart, "During")) %>% 
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples from 8/2
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

# Order the new ActionPhase so that it plots in the order Pre < During < Post
phase.order <- c("Before","During","After")
phyto.grp$ActionPhase <- factor(as.character(phyto.grp$ActionPhase), levels = phase.order)


## Create new data table to match SFEWS data
phyto.grp.SFEWS <- phyto.grp

## Remove extra columns
phyto.grp.SFEWS$WYType <- NULL
phyto.grp.SFEWS$FlowPulseType <- NULL
phyto.grp.SFEWS$NetFlowDays <- NULL
phyto.grp.SFEWS$Year <- NULL

# Remove samples not used in Jared's graphs
phyto.grp.SFEWS <- phyto.grp.SFEWS %>%
  filter(StationCode != "I80") %>%
  filter(StationCode != "RCS") %>%
  filter(StationCode != "RD22") 

# Remove Sept samples (not used in Jared's graphs)
# Jared's graphs didn't look at September
phyto.grp.SFEWS <- phyto.grp.SFEWS %>%
  filter(DateTime <= "2016-08-31") %>%
  filter(DateTime >= "2016-06-29")

## Create summary table
phyto.grp.SFEWS.table <- phyto.grp.SFEWS %>% 
  group_by(ActionPhase,StationCode, Group) %>%
  summarize(Conc = mean(Conc, na.rm = T)) %>%
  ungroup

## Replicate graph from SFEWS paper
plot <- ggplot(phyto.grp.SFEWS,
               aes(x = StationCode,
                   y = Conc,
                   fill = Group)) +
  geom_bar(data = phyto.grp.SFEWS, 
           position = "stack",  
           width = 0.4, 
           stat = "summary", 
           fun = "mean") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Station", 
       y = "Biovolume (um^3 per mL)", 
       title = "Recreation of Figure A5 (Frantzich et al., 2021)") 

plot +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF")) +
  facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h", scales = "free_y")

ggsave(path="plots",
       filename = "phyto_BV_2016_SFEWS.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=5, 
       dpi="print")

## Read in data copied from Jared's Graph
## Table PIVOT_BARGraph

jared <- read_csv("jared_graph_data.csv")

jared.l <- pivot_longer(jared, cols = 3:8,
                        names_to = "Group",
                        values_to = "Conc")

## Summarize total values to compare between Jared's data and data processed 
## using Ted's phyto script

jared.summary <- jared.l %>%
  group_by(ActionPhase, Site) %>%
  summarize(Total.BV.J = sum(Conc, na.rm = TRUE)) %>%
  ungroup
  
phyto.summary <- phyto.grp.SFEWS.table %>%
  group_by(ActionPhase, StationCode) %>%
  summarize(Total.BV = sum(Conc, na.rm = TRUE)) %>%
  ungroup

## Rename header to more easily join datasets
phyto.summary <- rename(phyto.summary, Site = StationCode)

## Join both datasets to compare side by side
total.BV.comparison  <- left_join(phyto.summary, jared.summary)

total.BV.comparison <- total.BV.comparison %>%
  mutate(BV.Diff = (Total.BV - Total.BV.J)/((Total.BV + Total.BV.J)/2)*100)

## Confirmed that data from original sheets matches Jared's!

### Convert data to Biomass and LCEFA to compare with Jared's data there

## Convert Biovolume (units um^3/mL) to Biomass (units = ug - C per L)
## Equation converts to pg - C so divide by 10^6 to get ug
## Multiply by 10^3 to convert from mL to L
## Overall conversion factor is divide by 10^3 to get ug-C per L

phyto.grp.SFEWS.w <- pivot_wider(phyto.grp.SFEWS, names_from = Group, values_from = Conc)

## Convert biovolume to biomass using relationships from Menden-Deuer and Lussard
## (2000) doi: 10.4319/lo.2000.45.3.0569
phyto.grp.BM <- phyto.grp.SFEWS.w %>%
  mutate(Diatoms.BM = 0.288*(Diatoms^0.811)/1e3) %>%
  mutate(Cyanobacteria.BM = 0.216*(Cyanobacteria^0.939)/1e3) %>%
  mutate(GreenAlgae.BM = 0.216*(`Green Algae`^0.939)/1e3) %>%
  mutate(GoldenAlgae.BM = 0.216*(`Golden Algae`^0.939)/1e3) %>%
  mutate(Cryptophytes.BM = 0.216*(Cryptophytes^0.939)/1e3) %>%
  mutate(Dinoflagellates.BM = 0.216*(Dinoflagellates^0.939)/1e3) %>%
  mutate(Other.BM = 0.216*(Other^0.939)/1e3)

## Remove Biovolume data from table 

phyto.grp.BM[6:12] <- NULL

## Calculate sum(LCEFA) from biomass data

## Pivot to longer table for biomass plots
phyto.grp.BM.l <- pivot_longer(phyto.grp.BM, cols = 6:12,
                               names_to = "Group", 
                               values_to = "Biomass")

## Order taxonomic groups
group.order <- c("Diatoms.BM","Cyanobacteria.BM","GreenAlgae.BM","Cryptophytes.BM","Ciliates.BM","Dinoflagellates.BM","GoldenAlgae.BM","Other.BM")
phyto.grp.BM.l$Group <- factor(as.character(phyto.grp.BM.l$Group), levels =  group.order)

## Summarize total values to compare between Jared's data and data processed 
## using Ted's phyto script

## Create summary table
phyto.grp.BM.table <- phyto.grp.BM.l %>% 
  group_by(ActionPhase,StationCode, Group) %>%
  summarize(Biomass = mean(Biomass, na.rm = T)) %>%
  ungroup

phyto.BM.summary <- phyto.grp.BM.table %>%
  group_by(ActionPhase, StationCode) %>%
  summarize(Total.BM = sum(Biomass, na.rm = TRUE)) %>%
  ungroup

## Plot biomass abundance
biomass.plot <- ggplot(phyto.grp.BM.l, 
                       aes(x = StationCode, 
                           y = Biomass, 
                           fill = Group)) +
  geom_bar(position = "stack", 
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Pulse Period", 
       y = "Biomass (ug-C per L)", 
       title = "Estimated Biomass of Phytoplankton Groups During Flow Pulses - ") 

biomass.plot +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h", scale = "free_y")

#### Summary table comparison shows that Ted's data matches Jared's data!


## Calculate LCEFA composition based on method in Galloway & Winder (2015) 
## doi: 10.1371/journal.pone.0130053

phyto.grp.LCEFA <- phyto.grp.BM %>%
  mutate(Diatoms.LCEFA = Diatoms.BM*2.77/100) %>%
  mutate(Cyanobacteria.LCEFA = Cyanobacteria.BM*0.02/100) %>%
  mutate(GreenAlgae.LCEFA = GreenAlgae.BM*0.52/100) %>%
  mutate(Cryptophytes.LCEFA = Cryptophytes.BM*2.13/100) %>%
  mutate(Dinoflagellates.LCEFA = Dinoflagellates.BM*3.82/100)

## Remove extra columns
phyto.grp.LCEFA[6:12] <- NULL
phyto.grp.LCEFA$Type <- NULL

## Pivot to longer table for LCEFA bar plots
phyto.grp.LCEFA.l <- pivot_longer(phyto.grp.LCEFA, cols = 5:9,
                                  names_to = "Group", 
                                  values_to = "LCEFA.ug.L")

## Sum total LCEFA
phyto.grp.LCEFA <- phyto.grp.LCEFA %>% 
  rowwise() %>% 
  mutate(Total.LCEFA = sum(c(Diatoms.LCEFA,Dinoflagellates.LCEFA,Cyanobacteria.LCEFA,GreenAlgae.LCEFA,Cryptophytes.LCEFA), na.rm = T))

## Set display order for LCEFA graphs
group.order.LCEFA <- c("Diatoms.LCEFA",
                       "Cyanobacteria.LCEFA",
                       "GreenAlgae.LCEFA",
                       "Cryptophytes.LCEFA",
                       "Dinoflagellates.LCEFA")
phyto.grp.LCEFA.l$Group <- factor(as.character(phyto.grp.LCEFA.l$Group), 
                                  levels =  group.order.LCEFA)


## Plot LCEFA Abundance

LCEFA.bar.plot <- ggplot(phyto.grp.LCEFA.l, 
                         aes(x = StationCode, 
                             y = LCEFA.ug.L, 
                             fill = Group)) +
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Pulse Period", 
       y = "LCEFA.ug.L (ug-C per L)", 
       title = "Estimated LCEFA of Phytoplankton Groups During Flow Pulses - 2016") 

LCEFA.bar.plot +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FFFF33")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h", scale = "free_y")
























