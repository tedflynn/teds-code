## Extracting FASTR data from 
## AEU's Yolo Bypass Phyto datasheets
## 2/3/2022

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Import AEU data files
phyto_files <- dir(path = "data/csv", pattern = "\\.csv", full.names = T)

phyto_all <- map_dfr(phyto_files, ~read_csv(.x))

## Read in files with non-standard headers individually
Dec2021 <- read_csv("data/oddballs/December 2021.csv")
Nov2021 <- read_csv("data/oddballs/November 2021.csv")
Sep2013 <- read_csv("data/oddballs/September 2013.csv")
Nov2013 <- read_csv("data/oddballs/November 2013.csv")

## Combine like oddball dfs
phyto2013 <- bind_rows(Sep2013, Nov2013)
phyto2021 <- bind_rows(Dec2021, Nov2021)

## Remove individual dfs
rm(Dec2021)
rm(Nov2021)
rm(Nov2013)
rm(Sep2013)

## Rename headers to match standard BSA headers
## Oddballs actually have the "correct" name of Total Cells 
## rather than the incorrect "Number of cells per unit"

phyto2013 <- phyto2013 %>%
  rename("Number of cells per unit" = "Total Cells Counted")

phyto2021 <- phyto2021 %>%
  rename("Number of cells per unit" = "Total Number of Cells")

phyto2021 <- phyto2021 %>%
  rename("Unit Abundance" = "Unit Abundance (# of Natural Units)")

## Remove GALD measurements
phyto_all$GALD <- NULL
phyto_all$`GALD 1` <- NULL
phyto2021$`GALD 1` <- NULL
phyto2013$GALD <- NULL

## Combine oddball files with others
phyto_all <- bind_rows(phyto_all, phyto2013)
phyto_all <- bind_rows(phyto_all, phyto2021)

## Remove pre-calculated Unit Density and blank columns
phyto_all[36:39] <- NULL

# Clean up column names
phyto <- phyto_all %>%
  clean_names(case = "big_camel")

## Remove empty rows
phyto <- phyto %>% filter_all(any_vars(!is.na(.)))

sort(table(phyto$BsaTin))

## Average all 10 biovolume measurements for each taxon
phyto <- phyto %>%
  rowwise() %>%
  mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T))

# Remove Individual Biovolume Columns
phyto[26:35] <- NULL

# Remove unneeded columns
phyto[21:25] <- NULL
phyto[5:11] <- NULL
phyto$MethodCode <- NULL
phyto$DiatomSoftBody <- NULL
phyto$Synonym <- NULL



## Get dates in the right format
## Some are 1/1/14 and others 1/1/2014
phyto$SampleDate <- mdy(phyto$SampleDate)

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


## Calculate Unit Density & Biovolume Density
phyto <- phyto %>%
  mutate(Unit.Density = UnitAbundance * Factor) %>%
  mutate(BV.Density = TotalCells * BV.Avg * Factor)

## Remove columns no longer needed
phyto$UnitAbundance <- NULL
phyto$TotalCells <- NULL
phyto$Factor <- NULL
phyto$BV.Avg <- NULL
phyto$Species <- NULL

## List of stations 

list(unique(phyto$StationCode))

## Fix site names
phyto$StationCode <- gsub("EZ6 SAC","EZ6",phyto$StationCode)
phyto$StationCode <- gsub("EZ6SAC","EZ6",phyto$StationCode)
phyto$StationCode <- gsub("EZ6SJR","EZ6-SJR",phyto$StationCode)
phyto$StationCode <- gsub("EZ2SAC","EZ2",phyto$StationCode)
phyto$StationCode <- gsub("EZ2 SAC","EZ2",phyto$StationCode)
phyto$StationCode <- gsub("EZ2SJR","EZ2-SJR",phyto$StationCode)
phyto$StationCode <- gsub("EZ2 SJR","EZ2-SJR",phyto$StationCode)
phyto$StationCode <- gsub("EZ6 SJR","EZ6-SJR",phyto$StationCode)
phyto$StationCode <- gsub("D16-Twitchell","D16",phyto$StationCode)
phyto$StationCode <- gsub("D16-Twitchel","D16",phyto$StationCode)
phyto$StationCode <- gsub("D16 - Twitchell","D16",phyto$StationCode)
phyto$StationCode <- gsub("D16 Twitchell","D16",phyto$StationCode)
phyto$StationCode <- gsub("NZ328","NZ325",phyto$StationCode) ## Typo in August 2019
phyto$StationCode <- gsub("C3A-HOOD","C3A",phyto$StationCode)
phyto$StationCode <- gsub("C3A Hood","C3A",phyto$StationCode)
phyto$StationCode <- gsub("C3A- Hood","C3A",phyto$StationCode)
phyto$StationCode <- gsub("C3A-Hood","C3A",phyto$StationCode)
phyto$StationCode <- gsub("NZ542","NZS42",phyto$StationCode)
phyto$StationCode <- gsub("E26","EZ6",phyto$StationCode)
phyto$StationCode <- gsub("E22","EZ2",phyto$StationCode) # Typo in May 2018

## Remove Microcystis tows at D19
phyto <- phyto %>% filter(StationCode != "D19 MC Tow")

## Print out all station IDs to flag which ones to remove
#all.stations <- sort(unique(phyto$StationCode))
#write(all.stations, file = "station_names.txt")

## Confirm station IDs
unique(phyto$StationCode)
table(phyto$StationCode)

# ## Import Stations listed as Upstream and Downstream
# region <- read_csv("upstream_downstream_stations.csv")
# region <- region %>% 
#   rename("Region" = "UpDown") %>%
#   rename("StationCode" = "Site")
# 
# region$Region <- factor(region$Region, levels = c("Upstream","Downstream"))

## In Fall 2016, taxonomists began classifying the species 
## Chroococcus microscopicus as Eucapsis microscopica. This is one of the most
## dominant species in this samples, so all taxa previously classified as 
## C. microscopicus will be re-named E. microscopica
## Need to fix because not all Chroococcus genera were renamed in this way, but 
## Chroococcus microscopicus is so dominant it works for now
#phyto$Taxon <- gsub("Chroococcus microscopicus","Eucapsis microscopica",phyto$Taxon)
#phyto$Genus <- gsub("Chroococcus","Eucapsis",phyto$Genus)

## Correct the genus label for a Chlorella entry
#phyto$Genus <- gsub("cf Chlorella","Chlorella",phyto$Genus)

sort(unique(phyto$Genus)) ## 238 unique genera

## Add column for year and month for highlighting data
phyto <- phyto %>%
  mutate(Year = year(phyto$DateTime))

phyto <- phyto %>%
  mutate(Month = month(phyto$DateTime, label = T))

## Order month in calendar order rather than (default) alphabetical
phyto$Month = factor(phyto$Month, levels = month.abb)


Aul <- phyto %>% 
  #filter(Year==2016) %>%
  filter(Genus == "Aulacoseira")

Aul$Year <- as.factor(Aul$Year)

x <- ggplot(Aul, aes(x = Year, y = BV.Density, color = Year)) +
  geom_jitter(width = 0.1) +
  scale_color_brewer(palette = "Set1")

x 
  #facet_wrap(StationCode ~ .)

y <- ggplot(Aul, aes(x = Month, y = log10(BV.Density+1), color = StationCode)) +
  geom_jitter(width = 0.1) 
  #scale_color_brewer(palette = "Set1")

y

write_csv(phyto, file = "phyto_EDI.csv")
write_csv(phyto2013, file = "phyto_2013.csv")

## Add higher-level taxonomy names
taxa <- read_csv("phyto_group_taxonomy.csv")
phyto <- left_join(phyto, taxa)

## Reorder columns
phyto <- phyto %>%
  relocate(Group, .after = Genus) %>%
  relocate(Year, .after = DateTime) %>%
  relocate(Month, .after = DateTime)

## Pivot to long format
## Summarize biovolume by genus
phyto.l <- pivot_longer(phyto, cols = 8:9, 
                        names_to = "Type", 
                        values_to = "Conc")

## Units for Density (unit and biovolume) are per mL, will convert to per L because
## final units of biomass and LCEFA will be per-liter. 
phyto.l <- phyto.l %>%
  mutate(Conc.per.L = Conc*10^3, .keep = "unused")

## Subset to biovolume data only
phyto.BV <- phyto.l %>%
  filter(Type == "BV.Density")

## Identify remove outliers
boxplot(phyto.BV$Conc.per.L)

# Identify 0.1% and 99.9% 
quartiles <- quantile(phyto.BV$Conc.per.L, probs=c(0.001, 0.999), na.rm = TRUE)

# List upper cutoff (99.9%)
cutoff <- quartiles[2]

# Filter dataset to show all taxa above this 99.9% cutoff
outliers <- phyto.BV %>%
   filter(Conc.per.L > cutoff)

list(outliers) ## 4 of top 6 are Spirogyra. 
phyto.BV %>% filter(Genus == "Spirogyra") # Only 5 total samples w/ Spirogyra

# Remove Spirogyra from dataset
# 4/5 are above 99.9% cutoff, 5 is also very very abundant
# These are benthic algae that clump, more likely to be "bullseye" samples
# that got a big blob or clumb
phyto.BV <- phyto.BV %>% filter(Genus != "Spirogyra")

## Remove 2013 data (very limited)
phyto.BV <- phyto.BV %>% filter(Year != 2013)

## Summarize by genus
phyto.BV.gen <- phyto.BV %>% 
  group_by(Type, Year, Month, DateTime, StationCode, Genus) %>%
  summarize(Conc.per.L = sum(Conc.per.L, na.rm = TRUE)) %>%
  ungroup

## Summarize by algal group
phyto.BV.grp <- phyto.BV %>% 
  group_by(Type, Year, Month, DateTime, StationCode, Group) %>%
  summarize(Conc.per.L = sum(Conc.per.L, na.rm = TRUE)) %>%
  ungroup

## Remove Type column
phyto.BV.grp$Type <- NULL
phyto.BV.gen$Type <- NULL

## Rename Conc header to BV.Density
phyto.BV.grp <- phyto.BV.grp %>% rename("BV.Density" = "Conc.per.L")
phyto.BV.gen <- phyto.BV.gen %>% rename("BV.Density" = "Conc.per.L")

## Add zeros to samples with no representatives in both data frames
# Group level
phyto.BV.grp.w <- pivot_wider(phyto.BV.grp, 
                              names_from = Group, 
                              values_from = BV.Density,
                              values_fill = 0)

phyto.BV.grp <- pivot_longer(phyto.BV.grp.w, cols = 5:last_col(),
                          names_to = "Group",
                          values_to = "BV.Density")
rm(phyto.BV.grp.w)

# Genus level
phyto.BV.gen.w <- pivot_wider(phyto.BV.gen, 
                              names_from = Genus, 
                              values_from = BV.Density,
                              values_fill = 0)

phyto.BV.gen <- pivot_longer(phyto.BV.gen.w, cols = 5:last_col(),
                             names_to = "Genus",
                             values_to = "BV.Density")

rm(phyto.BV.gen.w)

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

# Merge data from FlowDesignation Table (Water Year Type, Flow days and type)
# Filter only Pre-During-Post Flow Action Data. 
phyto.BV.grp <- inner_join(phyto.BV.grp,FlowDesignation, by = "Year")   
phyto.BV.grp <- phyto.BV.grp %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PreFlowEnd & DateTime < PostFlowStart, "During")) %>% ## added >= to avoid removing samples that fall on PreFlowEnd date
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples that fall on PostFlowStart date
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

phyto.BV.gen <- inner_join(phyto.BV.gen,FlowDesignation, by = "Year")   
phyto.BV.gen <- phyto.BV.gen %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PreFlowEnd & DateTime < PostFlowStart, "During")) %>% ## added >= to avoid removing samples that fall on PreFlowEnd date
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples that fall on PostFlowStart date
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

# Order the new ActionPhase so that it plots in the order Pre < During < Post
phase.order <- c("Before","During","After")
phyto.BV.grp$ActionPhase <- factor(as.character(phyto.BV.grp$ActionPhase), levels = phase.order)
phyto.BV.gen$ActionPhase <- factor(as.character(phyto.BV.gen$ActionPhase), levels = phase.order)

## Set group display order
group.order.BV <- c("Diatoms","Cyanobacteria","Green Algae","Cryptophytes","Ciliates","Dinoflagellates","Golden Algae","Other")
phyto.BV.grp$Group <- factor(as.character(phyto.BV.grp$Group), levels =  group.order.BV)

## Add Region to data frames
phyto.BV.grp <- left_join(phyto.BV.grp, region)
phyto.BV.gen <- left_join(phyto.BV.gen, region)

phyto.BV.grp <- phyto.BV.grp %>% relocate(Region, .after = StationCode)
phyto.BV.gen <- phyto.BV.gen %>% relocate(Region, .after = StationCode)

## Convert biovolume to biomass using relationships from Menden-Deuer and Lussard
## (2000) doi: 10.4319/lo.2000.45.3.0569
## Only relevant to group-level data
## Units of BV.Density are um^3 per L

temp <- pivot_wider(phyto.BV.grp, 
                    names_from = Group, 
                    values_from = BV.Density, 
                    values_fill = 0)

temp <- temp %>%
  mutate(Diatoms = 0.288*(Diatoms^0.811), .keep = "unused") %>%
  mutate(Cyanobacteria = 0.216*(Cyanobacteria^0.939), .keep = "unused") %>%
  mutate(`Green Algae` = 0.216*(`Green Algae`^0.939), .keep = "unused") %>%
  mutate(`Golden Algae` = 0.216*(`Golden Algae`^0.939), .keep = "unused") %>%
  mutate(Cryptophytes = 0.216*(Cryptophytes^0.939), .keep = "unused") %>%
  mutate(Dinoflagellates = 0.216*(Dinoflagellates^0.939), .keep = "unused") %>%
  mutate(Ciliates = 0.216*(Ciliates^0.939), .keep = "unused") %>%
  mutate(Other = 0.216*(Other^0.939), .keep = "unused")

## Pivot back to long format
phyto.BM.grp <- pivot_longer(temp, cols = 9:last_col(), 
                        names_to = "Group", 
                        values_to = "Biomass")

rm(temp)

## Convert pg to ug (ug-C per L)
phyto.BM.grp <- phyto.BM.grp %>% mutate(Biomass = Biomass / 10^6, .keep = "unused")

## Merge with Biovolume data
phyto.grp <- left_join(phyto.BM.grp, phyto.BV.grp)


## Calculate LCEFA composition based on method in Galloway & Winder (2015) 
## doi: 10.1371/journal.pone.0130053

temp <- pivot_wider(phyto.BM.grp, 
                    names_from = Group, 
                    values_from = Biomass, 
                    values_fill = 0)

temp <- temp %>%
  mutate(Diatoms = Diatoms*2.77/100, .keep = "unused") %>%
  mutate(Cyanobacteria = Cyanobacteria*0.02/100, .keep = "unused") %>%
  mutate(`Green Algae` = `Green Algae`*0.52/100, .keep = "unused") %>%
  mutate(Cryptophytes = Cryptophytes*2.13/100, .keep = "unused") %>%
  mutate(Dinoflagellates = Dinoflagellates*3.82/100, .keep = "unused")

temp$`Golden Algae` <- NULL
temp$Ciliates <- NULL
temp$Other <- NULL 

## Pivot to longer table for LCEFA bar plots
phyto.LCEFA.grp <- pivot_longer(temp, 
                                cols = 9:last_col(),
                                names_to = "Group", 
                                values_to = "LCEFA")

rm(temp)

## Merge LCEFA data with Biovolume and Biomass
phyto.grp <- left_join(phyto.grp, phyto.LCEFA.grp)

## Replace zeros in LCEFA data with zeros
phyto.grp$LCEFA <- phyto.grp$LCEFA %>% replace(is.na(.), 0)

## Sum together all data for each sampling point to get totals
temp <- pivot_longer(phyto.grp, 
                     cols = 10:last_col(), 
                     names_to = "DataType", 
                     values_to = "Amount")

temp <- temp %>%
  group_by(Year, Month, DateTime, StationCode, DataType) %>%
  summarize(Total = sum(Amount, na.rm = TRUE)) %>%
  ungroup

phyto.grp.sum <- pivot_wider(temp, names_from = "DataType", values_from = "Total", values_fill = 0)
phyto.grp.sum <- left_join(phyto.grp.sum, region)
phyto.grp.sum <- phyto.grp.sum %>% relocate(Region, .after = StationCode) 

## Add Flow Designation metadata
phyto.grp.sum <- inner_join(phyto.grp.sum,FlowDesignation, by = "Year")   
phyto.grp.sum <- phyto.grp.sum %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PreFlowEnd & DateTime < PostFlowStart, "During")) %>% ## added >= to avoid removing samples that fall on PreFlowEnd date
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples that fall on PostFlowStart date
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

## Rename column headers to reflect that they are totals
phyto.grp.sum <- phyto.grp.sum %>%
  rename("Total.LCEFA" = "LCEFA") %>%
  rename("Total.Biomass" = "Biomass") %>%
  rename("Total.BV.Density" = "BV.Density")

## Move data columns to end
phyto.grp.sum <- phyto.grp.sum %>%
  relocate(Total.LCEFA, .after = ActionPhase)

phyto.grp.sum <- phyto.grp.sum %>%
  relocate(Total.Biomass, .before = Total.LCEFA)

phyto.grp.sum <- phyto.grp.sum %>%
  relocate(Total.BV.Density, .before = Total.Biomass)

## Add in Flow Pulse Category Data
FlowPulseCategory <- read_csv("FlowPulseType.csv")
FlowPulseCategory <- FlowPulseCategory %>% rename("FlowPulseCategory" = "FlowPulseType")

## Add Flow Pulse Category to data frames to be exported
phyto.grp <- left_join(phyto.grp, FlowPulseCategory)
phyto.grp.sum <- left_join(phyto.grp.sum, FlowPulseCategory)
phyto.BV.gen <- left_join(phyto.BV.gen, FlowPulseCategory)

phyto.grp <- phyto.grp %>%
  relocate(FlowPulseCategory, .before = ActionPhase)

phyto.grp.sum <- phyto.grp.sum %>%
  relocate(FlowPulseCategory, .before = ActionPhase)

phyto.BV.gen <- phyto.BV.gen %>%
  relocate(FlowPulseCategory, .before = ActionPhase)

## Set order for ActionPhase to be displayed
phyto.grp.sum$ActionPhase <- factor(as.character(phyto.grp.sum$ActionPhase), levels = phase.order)

## Set order for stations to be displayed
phyto.grp$StationCode <- factor(as.character(phyto.grp$StationCode), levels = stations)
phyto.grp.sum$StationCode <- factor(as.character(phyto.grp.sum$StationCode), levels = stations)
phyto.BV.gen$StationCode <- factor(as.character(phyto.BV.gen$StationCode), levels = stations)

## Save data files
save(phyto, file = "phyto_raw.RData") 
save(phyto.grp, file = "phyto.grp.RData")
save(phyto.grp.sum, file = "phyto.grp.sum.RData")
save(phyto.BV.gen, file = "phyto.BV.gen.RData")
