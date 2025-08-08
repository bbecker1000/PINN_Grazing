#Target Fine Scale Data
library(tidyverse)
library(lme4)
library(readxl)
library (MASS)
library(sjPlot)
library(marginaleffects)

dev.off()
TargetData  <- read_excel("Data/TargetFineScaleData_Summary_stdmeas_latest.xlsx", 
                      col_types = c("numeric", "numeric", "text", 
                                  "text", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric"))

(TargetData)

unique(TargetData$Plot)
unique(TargetData$Subplot)

TargetData$Plot <- ifelse(TargetData$Plot == "G", "Grazed",
                           ifelse(TargetData$Plot == "S", "Seasonal",
                                  "Ungrazed"))



TargetData$Subplot <- ifelse(TargetData$Subplot == "C", "Control",
                        ifelse(TargetData$Subplot == "S", "Scraped + Seeded",
                               ifelse(TargetData$Subplot == "M", "Mowed",
                                      ifelse(TargetData$Subplot == "MS", "Mowed + Scraped + Seeded",
                                             TargetData$Subplot))))







TargetData$Plot <- factor(TargetData$Plot, levels=c('Ungrazed', 'Grazed', 'Seasonal'))
TargetData$Subplot <- factor(TargetData$Subplot, levels=c('Control', 'Scraped + Seeded', 'Mowed', 'Mowed + Scraped + Seeded'))
TargetData$Year.f <- as.factor(TargetData$Year) 
TargetData$Plot_SubPlot <- paste0(TargetData$Plot, " ", TargetData$Subplot)
unique(TargetData$Plot_SubPlot)
TargetData$Plot_SubPlot <- factor(TargetData$Plot_SubPlot, 
                                  levels = c("Ungrazed Control",  "Ungrazed Mowed",  "Ungrazed Mowed + Scraped + Seeded", 
                                             "Ungrazed Scraped + Seeded", "Grazed Control","Grazed Scraped + Seeded",  
                                             "Seasonal Control",  "Seasonal Scraped + Seeded"))
TargetData$Thatch_lbsAc <- as.integer(TargetData$Thatch_lbsAc)


# TargetData <- TargetData %>% filter(Plot_SubPlot != "UM")
# TargetData <- TargetData %>% filter(Plot_SubPlot != "UMS")

#make data long
TargetData_long <- 
  TargetData %>% 
        pivot_longer(!c(Year, Year.f, Block, Plot,  Subplot, Plot_SubPlot), 
               names_to = "Variable", values_to = "Value")

#quickplot
ggplot(TargetData_long, aes(x = Year.f, y = Value, color = Plot_SubPlot)) +
  geom_boxplot() +
  #geom_point() +
  facet_wrap(.~Variable)

#glmers

unique(TargetData_long$Variable)
[1] "VisObs_Av"            "AllYieldEstEst_lbsAc" "Thatch_lbsAc"        
[4] "AvVegHeight_cm"       "AvThatch_cm"          "BG_pct"              
[7] "MustardCovr_pct"      "MustardDens_m2"       "PoppyDens_m2"        
[10] "YStarDens_m2"         "GoldfieldDens_m2"     "OwlsClvDens_m2"      
[13] "TarweedDens_m2" 

names(TargetData[,-c(1:4)])


#thatch
#make integer for count data

hist(TargetData$Thatch_lbsAc)

#get nb dispersion parameter
summary(glm.nb(Thatch_lbsAc ~ Plot_SubPlot , data = TargetData))

#glmer
thatch.m1 <- glmer(Thatch_lbsAc ~ Year.f * Plot * Subplot + (1|Block) + (1|Year.f),
                   family = negative.binomial(0.797),  #from glm.nb
                   data = TargetData)
summary(thatch.m1)
plot(thatch.m1)
p.thatch_forest <- plot_model(thatch.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Thatch lbs/ac") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.thatch_forest 
#plot_model(thatch.m1, type = "int", terms = c("Year.f", "Subplot"))
#see plot 2 or
p.thatch_predict <- plot_predictions(thatch.m1, by = c("Year.f", "Subplot", "Plot")) +
  theme_gray(base_size = 16) + 
  ylab("Thatch lbs/acre") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.thatch_predict + ylim(0, 12000)


#bareground
hist(TargetData$BG)

bareground.m1 <- glmer(cbind(BG, 100) ~ Year.f * Plot * Subplot + (1|Block) + (1|Year.f),
                    family = binomial,  
                    data = TargetData)
summary(bareground.m1)
plot(bareground.m1)
p.bareground_forest <- plot_model(bareground.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Bare Ground Cover")  + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.bareground_forest
#plot_model(bareground.m1, type = "int", terms = c("Year.f", "Subplot","Plot"))
#see plot 2 or 
p.bareground_predict <- plot_predictions(bareground.m1, by = c("Year.f", "Subplot", "Plot")) +
  theme_gray(base_size = 16) + 
  ylab("Percent Bare Ground")  + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.bareground_predict



#mustard cover
hist(TargetData$Mustard)

TargetData$Mustard <- as.integer(TargetData$Mustard)

Mustard.m1 <- glmer(cbind(Mustard, 100) ~ Year.f * Plot * Subplot + (1|Block) + (1|Year.f),
                       family = binomial,  
                       data = TargetData)
summary(Mustard.m1)
plot(Mustard.m1)
p.Mustard_forest <- plot_model(Mustard.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Mustard Cover (No 2021 pre-treatment data)") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.Mustard_forest

#plot_model(Mustard.m1, type = "int", terms = c("Year.f", "Subplot","Plot"))
#see plot 2
p.Mustard_predict <- plot_predictions(Mustard.m1, by = c("Year.f", "Subplot", "Plot")) + ylim(0,0.5) +
  theme_gray(base_size = 16) + 
  ylab("Mustard Cover") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.Mustard_predict

#Seeding cover (data not yet available)


#AvVegHeight
hist(TargetData$AvVegHeight)

names(TargetData)

AvVegHeight.m1 <- glmer(AvVegHeight ~ Year.f * Plot * Subplot + (1|Block) + (1|Year.f),
                    family = Gamma(link = "log"),  
                    data = TargetData)
summary(AvVegHeight.m1)
plot(AvVegHeight.m1)
p.AvVegHeight_forest <- plot_model(AvVegHeight.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("AvVegHeight cm") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.AvVegHeight_forest

#plot_model(AvVegHeight.m1, type = "int", terms = c("Year.f", "Subplot","Plot"))
#see plot 2
p.AvVegHeight_predict <- plot_predictions(AvVegHeight.m1, by = c("Year.f", "Subplot", "Plot")) +
  ylim(0,110) +
  theme_gray(base_size = 16) + 
  ylab("Mean Veg Height") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.AvVegHeight_predict

#visual obstruction average - VisObs_Av
hist(TargetData$VisObs_Av)

names(TargetData)

VisObs_Av.m1 <- glmer(VisObs_Av ~ Year.f * Plot * Subplot + (1|Block) + (1|Year.f),
                    family = Gamma(link = "log"),  
                    data = TargetData)
summary(VisObs_Av.m1)
plot(VisObs_Av.m1)
p.VisObs_Av_forest <- plot_model(VisObs_Av.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("VisObs_Av") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.VisObs_Av_forest

#plot_model(VisObs_Av.m1, type = "int", terms = c("Year.f", "Subplot", "Plot")) 
#see plot 2
p.VisObs_Av_predict <- plot_predictions(VisObs_Av.m1, by = c("Year.f", "Subplot", "Plot"))   + 
  ylim(0,100) +
  theme_gray(base_size = 16) + 
  ylab("Mean Visual Obstruction") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.VisObs_Av_predict



library(patchwork)

p.thatch_forest + p.thatch_predict  
ggsave("Output/Thatch.png", width = 50, height = 15, units = "cm")
  p.bareground_forest + p.bareground_predict 
  ggsave("Output/Bareground.png", width = 50, height = 15, units = "cm")
  p.Mustard_forest + p.Mustard_predict 
  ggsave("Output/Mustard.png", width = 50, height = 15, units = "cm")
  p.AvVegHeight_forest + p.AvVegHeight_predict 
  ggsave("Output/Height.png", width = 50, height = 15, units = "cm")
  p.VisObs_Av_forest + p.VisObs_Av_predict 
  ggsave("Output/VisualObstruction.png", width = 50, height = 15, units = "cm")
  #plot_layout(ncol = 2)
  
    


# 1. use the plot_subplot groupings for all 
  
  plot(TargetData$AllYieldEstEst_lbsAc, TargetData$ForageYieldEst_lbsAc)


























  

