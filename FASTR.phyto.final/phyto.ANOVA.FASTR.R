## Calculate ANOVAs for FASTR
##

library("tidyverse");packageVersion("tidyverse")
library("ggpubr");packageVersion("ggpubr")
library("rstatix");packageVersion("rstatix")
library("car");packageVersion("car")
library("visreg");packageVersion("visreg")
library("emmeans");packageVersion("emmeans")

rm(list=ls()) #cleans workspace
theme_set(theme_bw())

setwd("C:/Users/tflynn/Documents/R/FASTR.phyto.redo")

load("yolo.phyto/phyto.BV.gen.RData")
load("yolo.phyto/phyto.grp.sum.RData")

## Change Year category to factor so it won't 
phyto.grp.sum$Year <- as.factor(phyto.grp.sum$Year)
phyto.grp.sum$FlowPulseType <- as.factor(phyto.grp.sum$FlowPulseType)

## Create QQ Plot to check for normality
ggqqplot(log10(phyto.grp.sum$Total.BV.Density))

## View histogram to check for normality
hist(log10(phyto.grp.sum$Total.BV.Density))

## Run Shapiro-Wilks test to check whether data is normally distributed
shapiro.test(log10(phyto.grp.sum$Total.BV.Density))

## Run Levene test to compare variance
phyto.grp.sum %>% levene_test(log10(Total.BV.Density)~FlowPulseType) # p = 0.901
phyto.grp.sum %>% levene_test(log10(Total.BV.Density)~Year) # p = 0.980
phyto.grp.sum %>% levene_test(log10(Total.BV.Density)~Region) # p = 0.0230

## Run 3-way ANOVA
phyto.grp.sum.aov <- phyto.grp.sum %>% anova_test(log10(Total.BV.Density) ~ Region*Year*ActionPhase)
phyto.grp.sum.aov

## Rosie Code
L1 <- lm(log10(Total.BV.Density)~ Region*Year + Region*ActionPhase + ActionPhase*Year, data = phyto.grp.sum)

summary(L1)

Anova(L1, type = 2)

visreg(L1)

visreg.AP.Region <- visreg(L1, xvar = "ActionPhase", by = "Year", gg = T)

visreg.AP.Region +
  facet_wrap(Year ~ ., ncol = 3, dir = "h")

ggsave(path="plots",
       filename = "visreg.AP.Region.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3.5,
       width=7, 
       dpi="print")

visreg(L1, xvar = "Region", by = "Year")
visreg(L1, xvar = "Region", by = "ActionPhase")
visreg(L1, xvar = "ActionPhase", by = "Region")


emmeans(L1, pairwise ~ Region:ActionPhase)
emmeans(L1, pairwise ~ ActionPhase)
emmeans(L1, pairwise ~ Year:ActionPhase)
