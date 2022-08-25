## Extracting FASTR data from 
## AEU's Yolo Bypass Phyto datasheets
## 2/3/2022

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("C:/Users/tflynn/Documents/R/FASTR.phyto.redo/yolo.phyto")

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Import AEU data files (comment out when finished)
#phyto_files <- dir(path = "data/csv", pattern = "\\.csv", full.names = T)
#phyto.all <- map_dfr(phyto_files, ~read_csv(.x))

## Save imported data files (takes a while)
#save(phyto.all, file = "phyto_raw.RData") 
load("phyto_raw.RData")

# Clean up column names
phyto <- phyto.all %>%
  clean_names(case = "big_camel")

## Remove GALD measurements
phyto[37:69] <- NULL

## Remove empty rows
phyto <- phyto %>% 
  filter_all(any_vars(!is.na(.)))

## Remove weird row with only a single zero in biovolume
phyto <- phyto %>% 
  drop_na(MethodCode)

## Average all 10 biovolume measurements for each taxon
phyto <- phyto %>%
  rowwise() %>%
  mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T))

# Remove Biovolume Columns
phyto[27:36] <- NULL

# Remove unneeded columns
phyto[21:26] <- NULL
phyto[5:11] <- NULL
phyto$MethodCode <- NULL
phyto$BsaTin <- NULL
phyto$DiatomSoftBody <- NULL
phyto$Synonym <- NULL

## Fix samples with missing times
## Just replace with 12:00:00 b/c aren't doing any time-based analyses
phyto <- phyto %>%
  replace_na(list(SampleTime = "12:00:00"))

## Get dates in the right format
## Some are 1/1/14 and others 1/1/2014
phyto$SampleDate <- parse_date_time(phyto$SampleDate, 
                                    c("%m/%d/%Y", "%m/%d/%y"))

## Combine date and time column
phyto <- phyto %>% 
  unite(DateTime, 
        c("SampleDate","SampleTime"), 
        sep = " ") #, remove = FALSE, na.rm = FALSE)

phyto$DateTime <- as_datetime(phyto$DateTime, 
                              tz = "US/Pacific",
                              format = c("%Y-%m-%d %H:%M:%OS"))

## Check for missing dates
phyto %>%
  filter(is.na(DateTime)) ## No missing dates

## Correct BSA header
phyto <- phyto %>%
  rename("TotalCells" = "NumberOfCellsPerUnit")

## List of stations used for FASTR
stations <- c("RMB","RCS","RD22","I80","LIS","STTD","BL5","LIB","RYI","RVB")

## Fix site names
phyto$StationCode <- gsub("SHER","SHR",phyto$StationCode)
phyto$StationCode <- gsub("BL-5","BL5",phyto$StationCode)

## Print out all station IDs to flag which ones to remove
all.stations <- sort(unique(phyto$StationCode))
write(all.stations, file = "station_names.txt")

## Read in station names with flags and merge
keepers <- read_csv("station_names_flagged.csv")
phyto <- left_join(phyto, keepers)

## Remove data unrelated to project
phyto <- phyto %>%
  filter(Flag == "Keep")

## Remove flag column
phyto$Flag <- NULL

## Confirm station IDs
unique(phyto$StationCode)
table(phyto$StationCode)

## Re-order stations from North to South
phyto$StationCode <- factor(as.character(phyto$StationCode), levels = stations)

## Import Stations listed as Upstream and Downstream
region <- read_csv("upstream_downstream_stations.csv")
region <- region %>% 
  rename("Region" = "UpDown") %>%
  rename("StationCode" = "Site")

region$Region <- factor(region$Region, levels = c("Upstream","Downstream"))

## In Fall 2016, taxonomists began classifying the species 
## Chroococcus microscopicus as Eucapsis microscopica. This is one of the most
## dominant species in this samples, so all taxa previously classified as 
## C. microscopicus will be re-named E. microscopica

phyto <- phyto %>% 
  mutate(Taxon = case_when(Taxon == 'Chroococcus microscopicus' ~ 'Eucapsis microscopica',
                           TRUE ~ Taxon))

phyto <- phyto %>% 
  mutate(Genus = case_when(Taxon == 'Eucapsis microscopica' ~ 'Eucapsis',
                           TRUE ~ Genus))

## The taxon Plagioselmis lacustris is inconsistently named, appearing sometimes as 
## Rhodomonas lacustris. Change to Rhodomonas lacustris to avoid confusion.

phyto <- phyto %>% 
  mutate(Taxon = case_when(Taxon == 'Plagioselmis lacustris' ~ 'Rhodomonas lacustris',
                           TRUE ~ Taxon))

phyto <- phyto %>% 
  mutate(Genus = case_when(Taxon == 'Rhodomonas lacustris' ~ 'Rhodomonas',
                           TRUE ~ Genus))

## Correct the genus label for a Chlorella entry
phyto$Genus <- gsub("cf Chlorella","Chlorella",phyto$Genus)

sort(unique(phyto$Genus)) ## 153 unique genera

## Add column for year and month for highlighting data
phyto <- phyto %>%
  mutate(Year = year(phyto$DateTime))

phyto <- phyto %>%
  mutate(Month = month(phyto$DateTime, label = T))

## Order month in calendar order rather than (default) alphabetical
phyto$Month = factor(phyto$Month, levels = month.abb)

## Add higher-level taxonomy names
taxa <- read_csv("phyto_group_taxonomy.csv")
phyto <- left_join(phyto, taxa)

## Check for NAs in group
unique(phyto$Group) ## no NAs

## Look to see which ones are NAs 
##phyto.NA <- phyto %>% filter(is.na(Group))

## Reorder columns
phyto <- phyto %>%
  relocate(Group, .after = Genus) %>%
  relocate(Year, .after = DateTime) %>%
  relocate(Month, .after = DateTime)

## Remove columns no longer needed
phyto$Species <- NULL

## Convert biovolume to biomass using relationships from Menden-Deuer and Lussard
## (2000) doi: 10.4319/lo.2000.45.3.0569
## Only relevant to group-level data
## Units of BV.Density are um^3 per L

## Calculate Biomass (pg-C per cell) from Biovolume (um^3 per cell)
phyto <- phyto %>% 
  mutate(Biomass.pg.C = case_when(Group == "Diatoms" ~ 0.288 * (BV.Avg^0.811),
                             Group != "Diatoms" ~ 0.216 * (BV.Avg^0.939)))

## Convert pg to ug (ug-C per L)
phyto <- phyto %>% mutate(Biomass.ug.C = Biomass.pg.C / 10^6, .keep = "unused")

## Calculate Unit Density, Cell Density, and Biovolume Density
phyto <- phyto %>%
  mutate(Units.per.mL = UnitAbundance * Factor) %>%
  mutate(BV.um3.per.mL = TotalCells * BV.Avg * Factor) %>%
  mutate(Cells.per.mL = TotalCells * Factor) 

## Calculate Biomass Density (pg-C per mL)
phyto <- phyto %>% 
  mutate(BM.ug.per.mL = Biomass.ug.C * Cells.per.mL)

## Remove columns no longer needed
phyto$UnitAbundance <- NULL
phyto$TotalCells <- NULL
phyto$Factor <- NULL
phyto$BV.Avg <- NULL
phyto$Biomass.ug.C <- NULL 

## Units for Density (unit, cell, biomass, and biovolume) are in per mL, will convert to per L because
## final units of biomass and LCEFA will be per liter. 
phyto <- phyto %>%
  mutate(across(8:11, ~ .x * 1000,.keep = "unused"))

## Rename headers b/c units are now in L
phyto <- phyto %>% rename("Units.per.L" = "Units.per.mL")
phyto <- phyto %>% rename("Cells.per.L" = "Cells.per.mL")
phyto <- phyto %>% rename("BV.um3.per.L" = "BV.um3.per.mL")
phyto <- phyto %>% rename("BM.ug.per.L" = "BM.ug.per.mL")

## Pivot to long format
## Summarize biovolume by genus
phyto.l <- phyto

phyto.l <- pivot_longer(phyto.l, cols = 8:11, 
                        names_to = "Type", 
                        values_to = "Conc")

## Subset to biovolume data only to analyze for outliers
phyto.BV <- phyto.l %>%
  filter(Type == "BV.um3.per.L")

## Identify remove outliers
boxplot(phyto.BV$Conc)

# Identify 0.1% and 99.9% 
quartiles <- quantile(phyto.BV$Conc, probs=c(0.001, 0.999), na.rm = TRUE)

# List upper cutoff (99.9%)
cutoff <- quartiles[2]

# Filter dataset to show all taxa above this 99.9% cutoff
outliers <- phyto.BV %>%
   filter(Conc > cutoff)

list(outliers) ## 4 of top 6 are Spirogyra. 
phyto.BV %>% filter(Genus == "Spirogyra")# Only 5 total samples w/ Spirogyra

# Remove Spirogyra from datasets
# 4/5 are above 99.9% cutoff, 5 is also very very abundant
# These are benthic algae that clump, more likely to be "bullseye" samples
# that got a big blob or clumb
#phyto.BV <- phyto.BV %>% filter(Genus != "Spirogyra")
phyto <- phyto %>% filter(Genus != "Spirogyra")

## Remove 2013 data (very limited)
#phyto.BV <- phyto.BV %>% filter(Year != 2013)
phyto <- phyto %>% filter(Year != 2013)

rm(phyto.BV)
rm(phyto.l)

## Summarize by algal group
phyto.grp <- phyto %>%
  group_by(Year, Month, DateTime, StationCode, Group) %>%
  summarize(across(Units.per.L:BM.ug.per.L, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

## Summarize by genus
phyto.gen <- phyto %>%
  group_by(Year, Month, DateTime, StationCode, Genus) %>%
  summarize(across(Units.per.L:BM.ug.per.L, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

## Calculate LCEFA composition based on method in Galloway & Winder (2015) 
## doi: 10.1371/journal.pone.0130053

phyto.grp <- phyto.grp %>% 
  mutate(LCEFA.per.L = case_when(Group == 'Diatoms' ~ BM.ug.per.L*2.77/100,
                                 Group == 'Cyanobacteria' ~ BM.ug.per.L*0.02/100,
                                 Group == 'Green Algae' ~ BM.ug.per.L*0.52/100,
                                 Group == 'Cryptophytes' ~ BM.ug.per.L*2.13/100,
                                 Group == 'Dinoflagellates' ~ BM.ug.per.L*3.82/100,
                                 TRUE ~ 0))

##### Cat Pien's Code for Merging Flow Designations #####
# 
# FlowDesignation <- read_csv("FlowDatesDesignations.csv")
# Update with 45 days limit on either end
### Update 5/27/22 TF added >= to two of the mutate commands to avoid removing 
### samples falling on the PreFlowEnd and PostFlowStart dates. 
FlowDesignation <- read_csv("FlowDatesDesignations_45days.csv")
FlowDesignation$PreFlowStart <- mdy(FlowDesignation$PreFlowStart)
FlowDesignation$PreFlowEnd <- mdy(FlowDesignation$PreFlowEnd)
FlowDesignation$PostFlowStart <- mdy(FlowDesignation$PostFlowStart)
FlowDesignation$PostFlowEnd <- mdy(FlowDesignation$PostFlowEnd)

FlowDesignation$NetFlowDays <- NULL

save(FlowDesignation, file = "FlowDesignation.RData")

# Merge data from FlowDesignation Table (Water Year Type, Flow days and type)
# Filter only Pre-During-Post Flow Action Data. 
phyto.grp <- inner_join(phyto.grp,FlowDesignation, by = "Year")   
phyto.grp <- phyto.grp %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PreFlowEnd & DateTime < PostFlowStart, "During")) %>% ## added >= to avoid removing samples that fall on PreFlowEnd date
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples that fall on PostFlowStart date
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

phyto.gen <- inner_join(phyto.gen,FlowDesignation, by = "Year")   
phyto.gen <- phyto.gen %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PreFlowEnd & DateTime < PostFlowStart, "During")) %>% ## added >= to avoid removing samples that fall on PreFlowEnd date
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples that fall on PostFlowStart date
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

# Order the new ActionPhase so that it plots in the order Pre < During < Post
phase.order <- c("Before","During","After")
phyto.grp$ActionPhase <- factor(as.character(phyto.grp$ActionPhase), levels = phase.order)
phyto.gen$ActionPhase <- factor(as.character(phyto.gen$ActionPhase), levels = phase.order)

## Set group display order
group.order <- c("Diatoms","Cyanobacteria","Green Algae","Cryptophytes","Ciliates","Dinoflagellates","Golden Algae","Other")
phyto.grp$Group <- factor(as.character(phyto.grp$Group), levels =  group.order)

## Add Region to data frames
phyto.grp <- left_join(phyto.grp, region)
phyto.gen <- left_join(phyto.gen, region)

phyto.grp <- phyto.grp %>% relocate(Region, .after = StationCode)
phyto.gen <- phyto.gen %>% relocate(Region, .after = StationCode)

## Add in Flow Pulse Category Data
FlowPulseCategory <- read_csv("FlowPulseType.csv")
FlowPulseCategory <- FlowPulseCategory %>% rename("FlowPulseCategory" = "FlowPulseType")

## Add Flow Pulse Category to data frames to be exported
phyto.grp <- left_join(phyto.grp, FlowPulseCategory)
phyto.gen <- left_join(phyto.gen, FlowPulseCategory)

## Relocate metadata columns
phyto.grp <- phyto.grp %>%
  relocate(FlowPulseCategory, .before = ActionPhase) %>%
  relocate(WYType:ActionPhase, .before = Units.per.L)

phyto.gen <- phyto.gen %>%
  relocate(FlowPulseCategory, .before = ActionPhase) %>%
  relocate(WYType:ActionPhase, .before = Units.per.L)

## Set order for stations to be displayed
phyto.grp$StationCode <- factor(as.character(phyto.grp$StationCode), levels = stations)
phyto.gen$StationCode <- factor(as.character(phyto.gen$StationCode), levels = stations)

## Separate out data frames by density type and add zeros
# Biovolume Density + Group
phyto.grp.BV <- phyto.grp %>% select(1:10,BV.um3.per.L)

# Add zeros for taxa that weren't detected
temp <- pivot_wider(phyto.grp.BV, 
                    names_from = Group,
                    values_from = BV.um3.per.L,
                    values_fill = 0)

phyto.grp.BV <- pivot_longer(temp,
                             cols = 10:last_col(),
                             names_to = "Group",
                             values_to = "BV.um3.per.L")

# Biomass Density + Group
phyto.grp.BM <- phyto.grp %>% select(1:10,BM.ug.per.L)

# Add zeros for taxa that weren't detected
temp <- pivot_wider(phyto.grp.BM, 
                    names_from = Group,
                    values_from = BM.ug.per.L,
                    values_fill = 0)

phyto.grp.BM <- pivot_longer(temp,
                             cols = 10:last_col(),
                             names_to = "Group",
                             values_to = "BM.ug.per.L")

# LCEFA Density + Group
phyto.grp.LCEFA <- phyto.grp %>% select(1:10,LCEFA.per.L)

# Add zeros for taxa that weren't detected
temp <- pivot_wider(phyto.grp.LCEFA, 
                    names_from = Group,
                    values_from = LCEFA.per.L,
                    values_fill = 0)

#Remove taxa that aren't used for LCEFA calculations
temp$Ciliates <- NULL
temp$Other <- NULL
temp$`Golden Algae` <- NULL

phyto.grp.LCEFA <- pivot_longer(temp,
                             cols = 10:last_col(),
                             names_to = "Group",
                             values_to = "LCEFA.per.L")

rm(temp)

# Biovolume Density + Genus
#phyto.gen.BV <- phyto.gen %>% select(1:10,BV.um3.per.L)
#
# Add zeros for taxa that weren't detected
# temp <- pivot_wider(phyto.gen.BV, 
#                     names_from = Genus,
#                     values_from = BV.um3.per.L,
#                     values_fill = 0)
# 
# phyto.gen.BV <- pivot_longer(temp,
#                              cols = 10:last_col(),
#                              names_to = "Genus",
#                              values_to = "BV.um3.per.L")
# 


## Summarize totals
phyto.sum <- phyto.grp %>%
  group_by(Year, Month, DateTime, StationCode, Region, WYType, FlowPulseType, FlowPulseCategory, ActionPhase) %>%
  summarize(across(Units.per.L:LCEFA.per.L, ~sum(.x, na.rm = TRUE))) %>%
  rename("Total.Units.per.L" = "Units.per.L") %>%
  rename("Total.Cells.per.L" = "Cells.per.L") %>%
  rename("Total.BV.per.L" = "BV.um3.per.L") %>%
  rename("Total.BM.per.L" = "BM.ug.per.L") %>%
  rename("Total.LCEFA.per.L" = "LCEFA.per.L") %>%
  ungroup

## Re-add group data to genus table
phyto.gen <- left_join(phyto.gen, taxa)

phyto.gen <- phyto.gen %>%
  relocate(Group, .before = Genus)

## Save data files
save(phyto.sum, file = "phyto.sum.RData")
save(phyto.gen, file = "phyto.gen.RData")
save(phyto.grp.BV, file = "phyto.grp.BV.RData")
save(phyto.grp.BM, file = "phyto.grp.BM.RData")
save(phyto.grp.LCEFA, file = "phyto.grp.LCEFA.RData")