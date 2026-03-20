#Target Fine Scale Data
library(tidyverse)
library(lme4)
library(readxl)
library (MASS)
library(sjPlot)
library(marginaleffects)
library(paletteer)

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
#get variables
unique(TargetData_long$Variable)
# [1] "VisObs_Av"            "AllYieldEstEst_lbsAc" "Thatch_lbsAc"        
# [4] "AvVegHeight_cm"       "AvThatch_cm"          "BG_pct"              
# [7] "MustardCovr_pct"      "MustardDens_m2"       "PoppyDens_m2"        
# [10] "YStarDens_m2"         "GoldfieldDens_m2"     "OwlsClvDens_m2"      
# [13] "TarweedDens_m2" 

# or get variables this way
names(TargetData[,-c(1:4)])


## add rain
RAIN <- read_csv("Data/PINN_PPT_rainfall.csv")
head(RAIN)
colnames(RAIN)[colnames(RAIN) == 'Water_Year'] <- 'Year'

TargetData <- left_join(TargetData, RAIN, by = "Year")

TargetData$Rain.f <- ifelse(TargetData$PPT_CM < 30, "Low", 
                            ifelse(TargetData$PPT_CM > 60, "High", 
                                   "Med"))

TargetData$Rain.f <- factor(TargetData$Rain.f,
                            levels = c("Low", "Med", "High"))



#mustard cover (HIIN) ---------------------
hist(TargetData$MustardCovr_pct)

# Check how many nonzero values you have

TargetData$MustardCovr_pct <- as.integer(TargetData$MustardCovr_pct)
# Remove NAs first
TargetData.Mustard <- TargetData[!is.na(TargetData$MustardCovr_pct), ]



# Create proportion column
TargetData.Mustard$MustardProp <- TargetData.Mustard$MustardCovr_pct / 100

# Refit model on clean data
Mustard.m1 <- glmer(MustardProp ~ Year.f + Plot + Subplot + (1|Block/Plot),
                    family = binomial,
                    weights = rep(100, nrow(TargetData.Mustard)),
                    data = TargetData.Mustard)
summary(Mustard.m1)
# Now plot_predictions should work
library(ggeffects)

p.Mustard_predict <- ggpredict(Mustard.m1, 
                               terms = c("Year.f", "Subplot", "Plot"),
                               type = "fixed") |>
  plot() +
  ylim(0, 0.5) +
  theme_gray(base_size = 16) +
  ylab("Mustard Cover") +
  xlab("Year") +
    scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")
p.Mustard_predict




#YStarDens_m2 (CESO)
#some na's and highly skewed --> logistic
# Convert table to data frame for plotting
YStar.summary <- TargetData.YStar %>%
  group_by(Year, Plot, Subplot) %>%
  summarise(
    n_present  = sum(YStarPresence, na.rm = TRUE),
    n_total    = n(),
    prop_present = mean(YStarPresence, na.rm = TRUE),
    .groups = "drop"
  )

subtrt_colors <- c(
  "Control"                  = "#8B0000",
  "Scraped + Seeded"         = "#4A4A4A",
  "Mowed"                    = "#E69A00",
  "Mowed + Scraped + Seeded" = "#4D7A3A"
)


YStar.summary.mean <- YStar.summary %>%
  group_by(Year, Subplot) %>%
  summarise(mean_prop = mean(prop_present, na.rm = TRUE),
            .groups = "drop")


ggplot(YStar.summary, aes(x = Year, y = prop_present, 
                                      color = Subplot, group = Subplot)) +
  geom_line(data = YStar.summary.mean, 
            aes(x = Year, y = mean_prop, color = Subplot, group = Subplot),
            linewidth = 1, linetype = "dashed") +
  geom_jitter(width = 0.05, height = 0.01, aes(size = n_present)) +
  scale_size_continuous(name = "# plots with YST") +
  theme_gray(base_size = 14) +
  ylab("Proportion of plots with YST present") +
  xlab("Year") +
  #ggtitle("Yellow Star Thistle Presence by Treatment") +
  scale_color_manual(values = subtrt_colors) +
  geom_vline(aes(xintercept = 2021.5), linetype = 4)




# 3. Native plant and wildlife habitat response (‘Results’)
# A. List figure / tables
# B. Thatch height / fuels
# C. Vegetation height
# D. Visual obstruction
# E. Bareground


#3B ThatchHeight

library(brms)

AvThatch_cm.m1.brms <- brm(
  AvThatch_cm ~ Year.f + Plot + Subplot + PPT_CM + (1|Block/Plot),
  data   = TargetData,
  family = hurdle_gamma(link = "log"),
  prior  = c(
    prior(normal(0, 1),  class = b),          # log scale: e^1 = ~3cm change, reasonable
    prior(normal(1, 1),  class = Intercept),  # log scale: e^1 = ~3cm, near your mean
    prior(cauchy(0, 1),  class = sd),         # tighter than 2.5 given small dataset
    prior(gamma(2, 0.5), class = shape)       # gamma shape parameter
  ),
  chains = 4,
  iter   = 4000,
  warmup = 1000,
  cores  = parallel::detectCores(),
  seed   = 123
)

summary(AvThatch_cm.m1.brms)
plot(AvThatch_cm.m1.brms)
pp_check(AvThatch_cm.m1.brms)  # posterior predictive check


#check
# 1. Posterior predictive check - does model fit data well?
pp_check(AvThatch_cm.m1.brms, ndraws = 100)

# 2. Fixed effects forest plot
plot(AvThatch_cm.m1.brms)

# 3. Conditional effects plots (one panel per predictor)
plot(conditional_effects(AvThatch_cm.m1.brms))

# 4. Specific predictor plots
conditional_effects(AvThatch_cm.m1.brms, effects = "Subplot")
conditional_effects(AvThatch_cm.m1.brms, effects = "PPT_CM")
conditional_effects(AvThatch_cm.m1.brms, effects = "Plot")

# 5. Interaction plot if needed
conditional_effects(AvThatch_cm.m1.brms, effects = "Subplot:Plot")

# 6. Using marginaleffects for more control
library(marginaleffects)
plot_predictions(AvThatch_cm.m1.brms, 
                 by = c("Year.f", "Subplot", "Plot"),
                 newdata = TargetData,
                 type = "response") +
  ylab("Thatch Height")

# 7. Clean ggplot version of conditional effects
ce <- conditional_effects(AvThatch_cm.m1.brms, effects = "Subplot")
plot(ce, plot = FALSE)[[1]] +
  theme_gray(base_size = 14) +
  ylab("Thatch Depth (cm)") +
  xlab("Treatment") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values = subtrt_colors)





#3C 
#AvVegHeight
hist(TargetData$AvVegHeight_cm)

names(TargetData)

AvVegHeight.m1 <- glmer(AvVegHeight_cm ~ Year.f + Plot + Subplot + (1|Block/Plot),
                        family = Gamma(link = "log"), nAGQ=0, #easier solution
                        data = TargetData)

#plot_model(AvVegHeight.m1, type = "int", terms = c("Year.f", "Subplot","Plot"))
#see plot 2
p.AvVegHeight_predict <- plot_predictions(AvVegHeight.m1, by = c("Year.f", "Subplot", "Plot")) +
  ylim(0,110) +
  theme_gray(base_size = 16) + 
  ylab("Mean Veg Height (cm)") + 
  scale_color_paletteer_d("wesanderson::Cavalcanti1") +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1")
p.AvVegHeight_predict



#3D Visual obstruction
#glmers
VisObs_Av.m1.brms <- brm(
  VisObs_Av ~ Year.f + Plot + Subplot + PPT_CM + (1|Block/Plot),
  data   = TargetData,
  family = Gamma(link = "log"),
  prior  = c(
    prior(normal(0, 1),  class = b),
    prior(normal(3, 1),  class = Intercept),  # log(20) ≈ 3, near your data mean
    prior(cauchy(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains = 4,
  iter   = 4000,
  warmup = 1000,
  cores  = parallel::detectCores(),
  seed   = 123,
  backend = "cmdstanr",
  threads_per_chain = 2 
)

pp_check(VisObs_Av.m1.brms, ndraws = 100)

# Effects plots
conditions <- data.frame(Plot = levels(factor(TargetData$Plot)))

ce <- conditional_effects(VisObs_Av.m1.brms,
                          effects = "Year.f:Subplot",
                          conditions = conditions)

plot(ce, plot = FALSE)[[1]] +
  theme_gray(base_size = 12) +
  ylab("Visual Obstruction (cm)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values = subtrt_colors) +
  facet_wrap(~Plot)


#3E bareground
#bareground

# Check what BGprop actually looks like
# Create proportion correctly from BG_pct directly
TargetData$BGprop <- TargetData$BG_pct / 100

# Verify
range(TargetData$BGprop, na.rm = TRUE)  # should be 0 to ~0.13
sum(TargetData$BGprop == 0, na.rm = TRUE)  # will be > 0

# Small offset to handle zeros for zero_one_inflated_beta
# OR use the squish approach
TargetData$BGprop_nz <- ifelse(TargetData$BGprop == 0, 0.0001, TargetData$BGprop)

bareground.m1.brms <- brm(
  BGprop_nz ~ Year.f + Plot + Subplot + PPT_CM + (1|Block) + (1|Year.f),
  data    = TargetData,
  family  = Beta(),  # no zeros after offset so simple beta works
  prior   = c(
    prior(normal(0, 1),   class = b),
    prior(normal(-3, 1),  class = Intercept),  # logit(0.03) ≈ -3.5
    prior(cauchy(0, 1),   class = sd),
    prior(gamma(2, 0.5),  class = phi)
  ),
  chains  = 4,
  iter    = 4000,
  warmup  = 3000,
  cores   = parallel::detectCores(),
  backend = "cmdstanr",
  threads_per_chain = 2, 
  control = list(adapt_delta = 0.93,   # default is 0.80
                 max_treedepth = 15),
  seed    = 123,
  threads = threading(16)
)

pp_check(bareground.m1.brms, ndraws = 100)

summary(bareground.m1.brms)  # check Rhat and ESS
#pairs(bareground.m1.brms, np = nuts_params(bareground.m1.brms))  # visualize divergence

saveRDS(bareground.m1.brms, "Output/bareground_model.rds")
#bareground.m1.brms <- readRDS("Output/bareground_model.rds")
saveRDS(AvThatch_cm.m1.brms, "Output/thatch_model.rds")
saveRDS(VisObs_Av.m1.brms, "Output/visobs_model.rds")

# Then remove from memory
rm(bareground.m1.brms, AvThatch_cm.m1.brms, VisObs_Av.m1.brms)
gc()  # force garbage collection






#AllYieldEstEst_lbsAc

hist(TargetData$AllYieldEstEst_lbsAc)


names(TargetData)


hist(TargetData$MustardDens_m2)
hist(TargetData$MustardCovr_pct)

## BB check units and troubleshoot why output units 

AllYieldEstEst_lbsAc.m1 <- glmer(AllYieldEstEst_lbsAc ~  Year.f + Plot + Subplot + PPT_CM + (1|Block/Plot),
                                 family = Gamma(link = "log"), nAGQ=1,
                     data = TargetData)
summary(AllYieldEstEst_lbsAc.m1)
plot(AllYieldEstEst_lbsAc.m1)
p.AllYieldEstEst_lbsAc.m1 <- plot_model(AllYieldEstEst_lbsAc.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Yield lbs/ac") + 
  scale_color_paletteer_d("wesanderson::Cavalcanti1") +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1")
p.AllYieldEstEst_lbsAc.m1 
#plot_model(thatch.m1, type = "int", terms = c("Year.f", "Subplot"))
#see plot 2 or
plot_predictions(VisObs_Av.m1, by = c("Year.f", "Subplot", "Plot")) +
  theme_gray(base_size = 14) + 
  ylab("AllYieldEstEst_lbsAc") + 
  xlab("Year") +
  scale_color_paletteer_d("wesanderson::Cavalcanti1") +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1") + 
  ylim(10,40)




#thatch
#make integer for count data

hist(TargetData$Thatch_lbsAc)

#get nb dispersion parameter
summary(glm.nb(Thatch_lbsAc ~ Plot_SubPlot , data = TargetData))





#glmer
thatch.m1 <- glmer(Thatch_lbsAc ~ Year.f + Plot * Subplot + (1|Block/Plot),
                   family = negative.binomial(0.797),  #from glm.nb
                   data = TargetData)
summary(thatch.m1)
plot(thatch.m1)
p.thatch_forest <- plot_model(thatch.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Thatch lbs/ac") + 
  scale_color_paletteer_d("wesanderson::Cavalcanti1") +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1")
p.thatch_forest 
#plot_model(thatch.m1, type = "int", terms = c("Year.f", "Subplot"))
#see plot 2 or
plot_predictions(thatch.m1, by = c("Year.f", "Subplot", "Plot")) +
  theme_gray(base_size = 14) + 
  ylab("Thatch lbs/acre") + 
  xlab("Year") +
  scale_color_paletteer_d("wesanderson::Cavalcanti1") +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1") + 
  ylim(0, 15000)


#bareground
hist(TargetData$BG_pct)
#convert to integers
TargetData$BG <- floor(TargetData$BG_pct)
hist(TargetData$BG)


bareground.m1 <- glmer(cbind(BG, 100) ~ Year.f * Plot * Subplot + (1|Block) + (1|Year.f),
                    family = binomial,  
                    data = TargetData)
summary(bareground.m1)
plot(bareground.m1)
plot_model(bareground.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("Bare Ground Cover")  + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")

#plot_model(bareground.m1, type = "int", terms = c("Year.f", "Subplot","Plot"))
#see plot 2 or 
plot_predictions(bareground.m1, by = c("Year.f", "Subplot", "Plot")) +
  theme_gray(base_size = 16) + 
  ylab("Percent Bare Ground")  + 
  scale_color_paletteer_d("wesanderson::Cavalcanti1") +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1") +
  ylim(0,0.1)

















#Seeding cover (data not yet available)












#AvVegHeight
hist(TargetData$AvVegHeight_cm)

names(TargetData)

AvVegHeight.m1 <- glmer(AvVegHeight_cm ~ Year.f + Plot + Subplot + (1|Block/Plot),
                    family = Gamma(link = "log"), nAGQ=0, #easier solution
                    data = TargetData)

summary(AvVegHeight.m1)
plot(AvVegHeight.m1)
plot_model(AvVegHeight.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("AvVegHeight cm") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")


#plot_model(AvVegHeight.m1, type = "int", terms = c("Year.f", "Subplot","Plot"))
#see plot 2
plot_predictions(AvVegHeight.m1, by = c("Year.f", "Subplot", "Plot")) +
  ylim(0,110) +
  theme_gray(base_size = 16) + 
  ylab("Mean Veg Height (cm)") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")


#visual obstruction average - VisObs_Av
hist(TargetData$VisObs_Av)

names(TargetData)

VisObs_Av.m1 <- glmer(VisObs_Av ~ Year.f + Plot * Subplot + (1|Block) + (1|Year.f),
                    family = Gamma(link = "log"),  
                    data = TargetData)
summary(VisObs_Av.m1)
plot(VisObs_Av.m1)
plot_model(VisObs_Av.m1) + geom_hline(yintercept = 1, linetype = 2) +
  ggtitle("VisObs_Av") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")


#plot_model(VisObs_Av.m1, type = "int", terms = c("Year.f", "Subplot", "Plot")) 
#see plot 2
plot_predictions(VisObs_Av.m1, by = c("Year.f", "Subplot", "Plot"))   + 
  ylim(0,100) +
  theme_gray(base_size = 16) + 
  ylab("Mean Visual Obstruction") + 
  scale_color_paletteer_d("wesanderson::GrandBudapest1") +
  scale_fill_paletteer_d("wesanderson::GrandBudapest1")




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


























  

