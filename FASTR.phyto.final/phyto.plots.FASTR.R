## Create data plots for FASTR synthesis Report 
## Using correct biovolume data (revised Feb 2022)
## 2/15/2022

library("tidyverse");packageVersion("tidyverse")
library("RColorBrewer");packageVersion("RColorBrewer")
library("vegan");packageVersion("vegan")

# Set working directory
setwd("C:/Users/tflynn/Documents/R/FASTR.phyto.redo/")

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

## Load biovolume density data at group and genus level
load("yolo.phyto/phyto.sum.RData")
load("yolo.phyto/phyto.gen.RData")
load("yolo.phyto/phyto.grp.BV.RData")
load("yolo.phyto/phyto.grp.BM.RData")
load("yolo.phyto/phyto.grp.LCEFA.RData")
load("yolo.phyto/phyto.grp.sum.error.RData")
load("yolo.phyto/FlowDesignation.RData")

phyto.gen$Group <- NULL 

## Create Biovolume-only data frame at genus level
phyto.gen.BV <- phyto.gen %>% select(1:10,BV.um3.per.L)

## Format Flow Designation data frame for graphing
FlowDesignation <- pivot_longer(FlowDesignation[1:5], cols = PreFlowStart:PostFlowEnd,
                                names_to = "Phase",
                                values_to = "Date")

FlowDesignation$Date <- as_datetime(FlowDesignation$Date,
                                        tz = "US/Pacific")

## Create custom palette
brewer.pal(n=8, name = "Set1")

# Set folder name to output graphs into
output <- "plots_Aug2022"

## Create box plots 
# Plot Upstream and Downstream Total BV by ActionPhase for each year
fig1 <- ggplot(data=phyto.sum, aes(y= log10(Total.BV.per.L), 
                                       x = Region,
                                       color = ActionPhase)) +
  geom_boxplot() 

fig1 +
  labs(title = "Total Phytoplankton Biovolume by Year",
       y = bquote(Log[10]~'Total Phyto Biovolume'~(µm^3~L^-1)), 
       x = "Sampling Region",
       color = "Pulse Period") +
  facet_wrap(Year ~ ., ncol = 2)  

ggsave(path = output,
       filename = "fig1_log_phyto_biovolume_by_year_and_AP.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Plot Total BV by Flow Pulse Type at Each Station
fig2 <- ggplot(data=phyto.sum, aes(y= log10(Total.BV.per.L), 
                                       x = FlowPulseCategory,
                                       color = ActionPhase)) +
  geom_boxplot()

fig2 +
  labs(title = "Total Phytoplankton Biovolume by Flow Pulse Type",
       y = "Log10 Total Phyto Biovolume (um^3 per L)", 
       x = "Flow Pulse Category") +
  facet_wrap(StationCode ~ ., ncol = 2)

ggsave(path = output,
       filename = "fig2_log_phyto_biovolume_by_site_and_pulse_type.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=8,
       width=6.5, 
       dpi="print")

## Make bar charts with error bars for each station 
fig3 <- ggplot(phyto.grp.sum.error, aes(x=Year, 
                                        y=Total.BV.per.L, 
                                        ymin=Total.BV.per.L-se, 
                                        ymax=Total.BV.per.L+se, 
                                        fill=ActionPhase)) +
  geom_bar(position=position_dodge(), 
           aes(y=Total.BV.per.L), 
           stat="identity", 
           color = "black") +
  geom_errorbar(position=position_dodge(width=0.9), 
                color="black", 
                width = 0.2) 

fig3 +
  ylab("Biovolume (um^3 per L)") + 
  xlab("Month") + 
  labs(title = "Phytoplankton Biovolume During Flow Actions") +
  facet_wrap(StationCode ~ ., ncol = 2, dir = "v") 

ggsave(path = output,
       filename = "fig3_phyto_BV_w_errorbars.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=7.5,
       width=6, 
       dpi="print")

## Create stacked bar chart for yearly biovolume by Action Phase
## and Upstream/Downstream
fig4 <- ggplot(phyto.grp.BV, aes(x = ActionPhase, 
                              y = BV.um3.per.L, 
                              fill = Group)) + 
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) 

fig4 + 
  labs(x = NULL, 
       y = bquote('Average Biovolume'~(µm^3~L^-1)), 
       title = paste0("Biovolume of Phytoplankton Groups During Flow Pulses")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_grid(Region~Year) # dir = v makes order of station go "north to south"

ggsave(path = output,
       filename = paste0("fig4_phyto_group_biovolume_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")

## Create biovolume plots for each year
years <- unique(phyto.grp.BV$Year) 
years <- sort(years, decreasing = F, na.last = T)

for (year in years) {
  df_temp <- phyto.grp.BV %>%
    filter(Year == year)
  
  biovolume.plot <- ggplot(phyto.grp.BV, 
                           aes(x = ActionPhase, 
                               y = BV.um3.per.L, 
                               fill = Group)) +
    geom_bar(data = subset(phyto.grp.BV, Year == year), 
             position = "stack",  
             width = 0.6, 
             stat = "summary", 
             fun = "mean") +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Pulse Period", 
         y = bquote('Average Biovolume'~(µm^3~L^-1)), 
         title = paste0("Biovolume of Phytoplankton Groups During Flow Pulses - ",year)) 
  
  biovolume.plot +
    scale_fill_manual(values = c("#E41A1C",
                                 "#377EB8",
                                 "#4DAF4A",
                                 "#984EA3",
                                 "#FF7F00",
                                 "#FFFF33",
                                 "#A65628",
                                 "#F781BF")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(StationCode ~ ., ncol = 5, dir = "h")
  
  ggsave(path = output,
         filename = paste0("phyto_avg_BV_by_station_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=7, 
         dpi="print")
  
  rm(df_temp)
  
}

## Create biovolume plots similar to Jared's

for (year in years) {
  df_temp <- phyto.grp.BV %>%
    filter(Year == year)
  
  biovolume.plot <- ggplot(phyto.grp.BV, 
                           aes(x = StationCode, 
                               y = BV.um3.per.L, 
                               fill = Group)) +
    geom_bar(data = subset(phyto.grp.BV, Year == year), 
             position = "stack",  
             width = 0.6, 
             stat = "summary", 
             fun = "mean") +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Station", 
         y = "Biovolume (um^3 per mL)", 
         title = paste0("Biovolume During Flow Pulses - ",year)) 
  
  biovolume.plot +
    scale_fill_manual(values = c("#E41A1C",
                                 "#377EB8",
                                 "#4DAF4A",
                                 "#984EA3",
                                 "#FF7F00",
                                 "#FFFF33",
                                 "#A65628",
                                 "#F781BF")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h")
  
  ggsave(path = output,
         filename = paste0("phyto_avg_BV_by_AP_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=5.5, 
         dpi="print")
  
  rm(df_temp)
  
}

#######################################
####           NMDS Calcs          ####
#######################################

# Convert long data to tabular form for nMDS calcs
# Sum together duplicate taxa (Desity of certain taxa with divergent sizes are calculated separately 
# according to Tiffany)
for (year in years) {
  genw <- pivot_wider(phyto.gen.BV, 
                      names_from = "Genus", 
                      values_from = "BV.um3.per.L",
                      values_fill = 0)
  
  genw <- genw %>% filter(Year == year)
  
  #look at number of observations per station
  table(genw$StationCode)
  
  # Calculate the nMDS using vegan 
  # A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
  # < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.
  phyto.NMDS <- metaMDS(
    comm = genw[c(10:142)],
    distance = "bray",
    k = 3,
    trymax = 10000
    #trace = F,
    #autotransform = F
  )
  
  #look at Shepard plot which shows scatter around the regression between the interpoint distances 
  #in the final configuration (i.e., the distances between each pair of communities) against their 
  #original dissimilarities.
  stressplot(phyto.NMDS)
  
  # Using the scores function from vegan to extract the site scores and convert to a data.frame
  data.scores <- as_tibble(scores(phyto.NMDS, display = "sites"))
  
  # Combine metadata with NMDS data scores to plot in ggplot
  meta <- genw %>% select(1:9)
  meta <- cbind(meta, data.scores)
  
  # Read in years as a character otherwise it shows up as a number and gets displayed as a gradient
  meta$Year <- as.character(meta$Year)
  
  # Plot NMDS in ggplot2
  ggplot(meta, aes(x = NMDS1, y = NMDS2, color = Region)) +
    geom_point(size = 3) +
    stat_ellipse() + 
    labs(title = paste0("NMDS - Biovolume - ",year)) +
    labs(color = "Region") +
    theme_bw()
  
  ggsave(path = output,
         filename = paste0("phyto_NMDS_biovolume_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3,
         width=4, 
         dpi="print")
}

## Plot NMDS by year and ActionPhase
for (year in years) {
  genw <- pivot_wider(phyto.gen.BV, 
                      names_from = "Genus", 
                      values_from = "BV.um3.per.L",
                      values_fill = 0)
  
  genw <- genw %>% filter(Year == year)
  
  #look at number of observations per station
  table(genw$StationCode)
  
  # Calculate the nMDS using vegan 
  # A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
  # < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.
  phyto.NMDS <- metaMDS(
    comm = genw[c(10:142)],
    distance = "bray",
    k = 3,
    trymax = 10
    #trace = F,
    #autotransform = F
  )
  
  #look at Shepard plot which shows scatter around the regression between the interpoint distances 
  #in the final configuration (i.e., the distances between each pair of communities) against their 
  #original dissimilarities.
  stressplot(phyto.NMDS)
  
  # Using the scores function from vegan to extract the site scores and convert to a data.frame
  data.scores <- as_tibble(scores(phyto.NMDS, display = "sites"))
  
  # Combine metadata with NMDS data scores to plot in ggplot
  meta <- genw %>% select(1:9)
  meta <- cbind(meta, data.scores)
  
  # Read in years as a character otherwise it shows up as a number and gets displayed as a gradient
  meta$Year <- as.character(meta$Year)
  
  # Plot NMDS in ggplot2
  ggplot(meta, aes(x = NMDS1, y = NMDS2, color = ActionPhase, shape = ActionPhase)) +
    geom_point(size = 3) +
    stat_ellipse() + 
    labs(title = paste0("NMDS - Biovolume - ",year)) +
    facet_wrap(Year ~ ., ncol = 2) +
    theme_bw()
  
  ggsave(path = output,
         filename = paste0("phyto_NMDS_biovolume_by_AP_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3,
         width=4, 
         dpi="print")
}

## Plot NMDS of all years by Region
genw <- pivot_wider(phyto.gen.BV, 
                    names_from = "Genus", 
                    values_from = "BV.um3.per.L",
                    values_fill = 0)

#look at number of observations per station
table(genw$StationCode)

# Calculate the nMDS using vegan 
# A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
# < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.
phyto.NMDS <- metaMDS(
  comm = genw[c(10:142)],
  distance = "bray",
  k = 3,
  trymax = 10
  #trace = F,
  #autotransform = F
)

#look at Shepard plot which shows scatter around the regression between the interpoint distances 
#in the final configuration (i.e., the distances between each pair of communities) against their 
#original dissimilarities.
stressplot(phyto.NMDS)

# Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores <- as_tibble(scores(phyto.NMDS, display = "sites"))

# Combine metadata with NMDS data scores to plot in ggplot
meta <- genw %>% select(1:9)
meta <- cbind(meta, data.scores)

# Read in years as a character otherwise it shows up as a number and gets displayed as a gradient
meta$Year <- as.character(meta$Year)

# Plot NMDS in ggplot2
ggplot(meta, aes(x = NMDS1, y = NMDS2, color = Region, shape = Region)) +
  geom_point(size = 3) +
  stat_ellipse() + 
  labs(title = "NMDS - Biovolume - All") +
  facet_wrap(ActionPhase ~ ., ncol = 1) +
  theme_bw()

ggsave(path = output,
       filename = paste0("phyto_NMDS_biovolume_all_by_AP_and Region.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=8,
       width=5, 
       dpi="print")

## Compare Cyclotella upstream and downstream
phyto.gen.BV.Cyc <- phyto.gen.BV %>% filter(Genus == "Cyclotella")

cyc.plot <- ggplot(phyto.gen.BV.Cyc, aes(x = Region, 
                                         y = log10(BV.um3.per.L+1))) +
  geom_jitter()

cyc.plot +
  facet_wrap(Year ~ ., ncol = 2)

# Time-series plots of Aulacoseira sp. at each station
## Aulacoseira was found in 2012 and 2016 blooms
phyto.gen.BV.2016 <- phyto.gen.BV %>% filter(phyto.gen.BV$Year==2016)

Aul <- ggplot(data = phyto.gen.BV.2016, aes(x = DateTime, y = BV.um3.per.L, color = Genus)) +
  geom_point(data = subset(phyto.gen.BV.2016, Genus == "Aulacoseira")) +
  geom_point(data = subset(phyto.gen.BV.2016, Genus == "Eucapsis")) +
  annotate("rect", fill = "gray", alpha = 0.5,
           xmin = as.POSIXct("2016-07-12 17:00:00"), xmax = as.POSIXct("2016-08-01 17:00:00"),
           ymin = -Inf, ymax = Inf)

Aul + 
  labs (x = "Date", y = "Biovolume (um^3 per mL)", title = "Biovolume of Aulacoseira diatoms - 2016") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(StationCode ~ ., ncol = 5) 

ggsave(path = output,
       filename = "aulacoseira_biovolume_by_station_2016.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=7.5, 
       dpi="print")

## Create stacked bar chart for yearly LCEFA abundance by Action Phase
## and Upstream/Downstream
fig12 <- ggplot(phyto.grp.LCEFA, aes(x = ActionPhase, 
                              y = LCEFA.per.L, 
                              fill = Group)) + 
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) 

fig12 + 
  labs(x = NULL, 
       y = bquote('Average LCEFA'~(µg~L^-1)), 
       title = paste0("Estimated Mass of LCEFA for Phytoplankton Groups During Flow Pulses")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#A65628")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_grid(Region~Year) # dir = v makes order of station go "north to south"

ggsave(path = output,
       filename = paste0("fig12_phyto_group_LCEFA_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")

## Make Diatom-specific plots for 2016
phyto.dia.BV.2016 <- phyto.gen.BV %>% 
  filter(Year==2016) %>%
  filter(Group == "Diatoms")

phyto.dia.BV.2016 <- phyto.dia.BV.2016 %>%
  mutate(Type = case_when(Genus == 'Aulacoseira' ~ 'Aulacoseira',
                          Genus == 'Cyclotella' ~ 'Cyclotella',
                          Genus == 'Ulnaria' ~ 'Ulnaria',
                          TRUE ~ 'Other'))

## Set group display order
type.order <- c("Aulacoseira","Cyclotella","Ulnaria","Other")
phyto.dia.BV.2016$Type <- factor(as.character(phyto.dia.BV.2016$Type), levels =  type.order)

Dia.2016 <- ggplot(data = phyto.dia.BV.2016, aes(x = StationCode, y = BV.um3.per.L, color = Type)) +
  geom_jitter(width = 0.1, size = 2) +
  scale_color_brewer(palette = "Set1")

Dia.2016 + 
  labs (x = "Date", y = "Biovolume (um^3 per mL)", title = "Biovolume of Diatoms - 2016") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(ActionPhase ~ ., ncol = 1) 

ggsave(path = output,
       filename = "jitter_plot_diatoms_2016.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")

### Summarize data to get raw numbers for paper
## Summarize by group
phyto.LCEFA.RA <- phyto.LCEFA.grp %>% 
  group_by(Year, Region, ActionPhase, Group) %>%
  summarize(LCEFA.Total = mean(LCEFA, na.rm = TRUE)) %>%
  ungroup

phyto.LCEFA.RA <- phyto.LCEFA.RA %>%
  group_by(Year, Region, ActionPhase) %>%
  mutate(LCEFA.RA = LCEFA.Total / sum(LCEFA.Total)) %>%
  ungroup

phyto.BV.RA <- phyto.grp %>% 
  group_by(Year, Region, ActionPhase, Group) %>%
  summarize(BV.Total = mean(BV.Density, na.rm = TRUE)) %>%
  ungroup

phyto.BV.RA <- phyto.BV.RA %>%
  group_by(Year, Region, ActionPhase) %>%
  mutate(BV.RA = BV.Total / sum(BV.Total)) %>%
  ungroup

