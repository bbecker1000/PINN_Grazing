library(tidyverse)
library(readr)
library(readxl)
library(vegan)
library(ggordiplots)
library(ggrepel)
library(cowplot)
library(lme4)
library(glmmTMB)
library(sjPlot)
library(broom)

#import
SumMust_Height <- read_excel("Data/SumMust_PlusVegHeight.xlsx")

# Mustard = mustard cover
# AvVegHeight = all veg (mostly mustard)


#remove MS and S seed treatments
SumMust_Height <- SumMust_Height %>% filter(VegTrt != "U" | SeedTrt != "MS")
SumMust_Height <- SumMust_Height %>% filter(VegTrt != "U" | SeedTrt != "S")
SumMust_Height$CombinedTreatment <- paste0(SumMust_Height$VegTrt, SumMust_Height$SeedTrt)

#add rainfall
RAIN <- read_csv("Data/PINN_PPT_rainfall.csv")
head(RAIN)
colnames(RAIN)[colnames(RAIN) == 'Water_Year'] <- 'Year'
SumMust_Height <- left_join(SumMust_Height, RAIN, by = "Year")
hist(SumMust_Height$PPT_CM)


#make some rain factors
SumMust_Height$Rain.f <- ifelse(SumMust_Height$PPT_CM < 30, "Low", 
                            ifelse(Wide_6.env$PPT_CM > 60, "High", 
                                   "Med"))

SumMust_Height$Rain.f <- factor(SumMust_Height$Rain.f,
                            levels = c("Low", "Med", "High"))


#make some pre-post factors
# I think no pre-data on this one?  2021 was treatment year? or was 2022 sampled just 
SumMust_Height$PrePost.f <- ifelse(SumMust_Height$Year == 2021, "Pre", "Post")

SumMust_Height$PrePost.f <- factor(SumMust_Height$PrePost.f,
                               levels = c("Pre", "Post"))

unique(SumMust_Height$VegTrt)
unique(SumMust_Height$SeedTrt)



#setup data types
SumMust_Height$Year.s <- as.factor(SumMust_Height$Year-2021)
SumMust_Height$Year.f <- as.factor(SumMust_Height$Year)
SumMust_Height$Mustard.r <- as.integer(round(SumMust_Height$Mustard))
SumMust_Height$VegTrt <- factor(SumMust_Height$VegTrt, levels = c("U", "G", "S"))
SumMust_Height$SeedTrt <- factor(SumMust_Height$SeedTrt, levels = c("C", "G", "S", "MS"))
SumMust_Height$AvVegHeight <- as.numeric(SumMust_Height$AvVegHeight)

#plot
ggplot(SumMust_Height, aes(Year.f, AvVegHeight, color = CombinedTreatment)) + 
  geom_boxplot() +
  geom_point(alpha = 0.7, position = position_dodge(0.75)) +
  ylim(0,120)

#glmm assuming out of 100 points and random effect for block
m2 <- glmer(AvVegHeight ~ VegTrt * SeedTrt * Year.f + PPT_CM + (1|Block) + (1|Year),
               family = Gamma(link = "log"), #similar answers with normal, but gamma makes more sense)
               data = SumMust_Height)
summary(m2)

# residual plot
plot(m2)
plot_model(m2, type = "resid")
plot_model(m2, type = "diag")

#forest plot
plot_model(m2) +
  geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Summer Mustard Height")

#interaction plot
plot_model(m2, type = "int", terms = c("VegTrt", "SeedTrt"))

plot_model(m2, type = "int", terms = c("VegTrt", "SeedTrt"))

```