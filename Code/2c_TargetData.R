# ============================================================
# TARGET FINE SCALE DATA - MODELS AND PLOTS
# ============================================================

library(tidyverse)
library(lme4)
library(readxl)
library(MASS)
library(marginaleffects)
library(paletteer)
library(DHARMa)
library(ggeffects)
library(brms)
library(cmdstanr)
library(glmmTMB)
library(patchwork)

# Safe dev.off() — won't error if no device is open
tryCatch(dev.off(), error = function(e) invisible(NULL))
# ============================================================
# LOAD DATA
# ============================================================
TargetData <- read_excel(
  "Data/TargetFineScaleData_Summary_stdmeas_latest.xlsx",
  col_types = c("numeric", "numeric", "text", "text",
                "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric", "numeric")
)

# ============================================================
# DATA WRANGLING
# ============================================================
TargetData <- TargetData %>%
  mutate(
    Plot = case_when(
      Plot == "G" ~ "Grazed",
      Plot == "S" ~ "Seasonal",
      .default    = "Ungrazed"
    ),
    Subplot = case_when(
      Subplot == "C"  ~ "Control",
      Subplot == "S"  ~ "Scraped + Seeded",
      Subplot == "M"  ~ "Mowed",
      Subplot == "MS" ~ "Mowed + Scraped + Seeded",
      .default = Subplot
    ),
    Plot    = factor(Plot,
                     levels = c("Ungrazed", "Grazed", "Seasonal")),
    Subplot = factor(Subplot,
                     levels = c("Control", "Scraped + Seeded",
                                "Mowed", "Mowed + Scraped + Seeded")),
    Year.f       = as.factor(Year),
    Plot_SubPlot = factor(
      paste0(Plot, " ", Subplot),
      levels = c("Ungrazed Control", "Ungrazed Mowed",
                 "Ungrazed Mowed + Scraped + Seeded",
                 "Ungrazed Scraped + Seeded", "Grazed Control",
                 "Grazed Scraped + Seeded", "Seasonal Control",
                 "Seasonal Scraped + Seeded")
    ),
    Thatch_lbsAc = as.integer(Thatch_lbsAc),
    # Combined Treatment variable — 8 valid combinations only
    Treatment = factor(
      case_when(
        Plot == "Ungrazed" & Subplot == "Control"                  ~ "Ungrazed",
        Plot == "Ungrazed" & Subplot == "Scraped + Seeded"         ~ "Ungrazed + Scraped + Seeded",
        Plot == "Ungrazed" & Subplot == "Mowed"                    ~ "Ungrazed + Mowed",
        Plot == "Ungrazed" & Subplot == "Mowed + Scraped + Seeded" ~ "Ungrazed + Mowed + Scraped + Seeded",
        Plot == "Grazed"   & Subplot == "Control"                  ~ "Grazed",
        Plot == "Grazed"   & Subplot == "Scraped + Seeded"         ~ "Grazed + Scraped + Seeded",
        Plot == "Seasonal" & Subplot == "Control"                  ~ "Seasonal Graze",
        Plot == "Seasonal" & Subplot == "Scraped + Seeded"         ~ "Seasonal Graze + Scraped + Seeded"
      ),
      levels = c(
        "Ungrazed",
        "Ungrazed + Scraped + Seeded",
        "Ungrazed + Mowed",
        "Ungrazed + Mowed + Scraped + Seeded",
        "Grazed",
        "Grazed + Scraped + Seeded",
        "Seasonal Graze",
        "Seasonal Graze + Scraped + Seeded"
      )
    )
  )

# Verify
cat("Treatment levels:\n")
print(levels(TargetData$Treatment))
print(table(TargetData$Treatment))

# Add rainfall
RAIN <- read_csv("Data/PINN_PPT_rainfall.csv") %>%
  rename(Year = Water_Year)

TargetData <- TargetData %>%
  left_join(RAIN, by = "Year") %>%
  mutate(
    Rain.f = case_when(
      PPT_CM < 30 ~ "Low",
      PPT_CM > 60 ~ "High",
      .default    = "Med"
    ),
    Rain.f    = factor(Rain.f, levels = c("Low", "Med", "High")),
    BGprop    = BG_pct / 100,
    BGprop_nz = ifelse(BGprop == 0, 0.0001, BGprop)
  )

# ============================================================
# COLOR PALETTES AND SHARED SETTINGS
# ============================================================
subtreatment_colors <- c(
  "Ungrazed"                            = "#08519C",
  "Ungrazed + Scraped + Seeded"         = "#4292C6",
  "Ungrazed + Mowed"                    = "#9ECAE1",
  "Ungrazed + Mowed + Scraped + Seeded" = "#C6DBEF",
  "Grazed"                              = "#A50F15",
  "Grazed + Scraped + Seeded"           = "#FB6A4A",
  "Seasonal Graze"                      = "#238B45",
  "Seasonal Graze + Scraped + Seeded"   = "#74C476"
)

# Shared brms settings
brms_cores   <- 4
brms_threads <- threading(16)
brms_backend <- "cmdstanr"
brms_seed    <- 123

# Shared plot theme
theme_pinn <- theme_gray(base_size = 14)

# Shared legend theme
legend_bottom <- theme(
  legend.position = "bottom",
  legend.key.size = unit(0.5, "cm"),
  legend.text     = element_text(size = 8),
  axis.text.x     = element_text(angle = 45, hjust = 1)
)

# ============================================================
# BUILD VALID NEWDATA GRIDS
# ============================================================
valid_newdata <- TargetData %>%
  distinct(Year.f, Treatment, .keep_all = TRUE) %>%
  arrange(Year.f, Treatment)
cat("Valid newdata grid:", nrow(valid_newdata), "rows\n")

valid_newdata.Mustard <- TargetData %>%
  filter(!is.na(MustardCovr_pct)) %>%
  mutate(
    MustardCovr_pct = as.integer(MustardCovr_pct),
    MustardProp     = MustardCovr_pct / 100,
    MustardProp_nz  = ifelse(MustardProp == 0, 0.0001, MustardProp)
  ) %>%
  distinct(Year.f, Treatment, .keep_all = TRUE) %>%
  arrange(Year.f, Treatment)

valid_newdata.MustardDens <- TargetData %>%
  filter(!is.na(MustardDens_m2)) %>%
  distinct(Year.f, Treatment, .keep_all = TRUE) %>%
  arrange(Year.f, Treatment)

# ============================================================
# QUICK OVERVIEW PLOT
# ============================================================
TargetData_long <- TargetData %>%
  dplyr::select(Year, Year.f, Block, Plot, Subplot,
                Plot_SubPlot, Treatment,
                VisObs_Av, AllYieldEstEst_lbsAc, Thatch_lbsAc,
                AvVegHeight_cm, AvThatch_cm, BG_pct,
                MustardCovr_pct, MustardDens_m2, PoppyDens_m2,
                YStarDens_m2, GoldfieldDens_m2, OwlsClvDens_m2,
                TarweedDens_m2) %>%
  pivot_longer(
    !c(Year, Year.f, Block, Plot, Subplot, Plot_SubPlot, Treatment),
    names_to  = "Variable",
    values_to = "Value"
  )

ggplot(TargetData_long,
       aes(x = Year.f, y = Value, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(. ~ Variable, scales = "free_y") +
  scale_color_manual(values = subtreatment_colors) +
  theme_pinn +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 45, hjust = 1))

# ============================================================
# HELPER FUNCTION: brms conditional_effects → line plot
# ============================================================
# Updated make_ce_plot — newdata passed via conditions instead
make_ce_plot <- function(model, title, ylab,
                         color_values  = subtreatment_colors,
                         ylim_vals     = NULL,
                         show_vline    = TRUE,
                         all_years     = c("2021", "2022", "2023", "2024", "2025")) {
  ce <- conditional_effects(
    model,
    effects    = "Year.f:Treatment",
    re_formula = NA,
    robust     = TRUE
  )
  
  p <- plot(ce, plot = FALSE)[[1]] +
    theme_pinn +
    legend_bottom +
    ylab(ylab) +
    xlab("Year") +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values  = color_values) +
    # Force x-axis to show all years even if model missing 2021
    scale_x_discrete(limits = all_years,
                     drop   = FALSE) +
    ggtitle(title)
  
  # Vline always at 1.5 (between 2021 and 2022) since x-axis is now fixed
  if (show_vline) {
    p <- p + geom_vline(xintercept = 1.5,
                        linetype   = 4,
                        color      = "gray40")
  }
  
  if (!is.null(ylim_vals)) p <- p + ylim(ylim_vals)
  p
}
# ============================================================
# 1. MUSTARD CANOPY GAP (HIIN) - zero_inflated_beta brms
# ============================================================
hist(TargetData$MustardCovr_pct)

TargetData.Mustard <- TargetData %>%
  filter(!is.na(MustardCovr_pct)) %>%
  mutate(
    MustardCovr_pct = as.integer(MustardCovr_pct),
    MustardProp     = MustardCovr_pct / 100,
    MustardProp_nz  = ifelse(MustardProp == 0, 0.0001, MustardProp)
  )

cat("Mustard zeros:", sum(TargetData.Mustard$MustardProp == 0), "\n")
cat("Mustard ones:",  sum(TargetData.Mustard$MustardProp == 1), "\n")

Mustard.m1.brms <- brm(
  MustardProp_nz ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData.Mustard,
  family  = zero_inflated_beta(),
  prior   = c(
    prior(normal(0, 1),   class = b),
    prior(normal(-2, 1),  class = Intercept),
    prior(normal(0, 1),   class = sd),
    prior(exponential(1), class = phi)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed,
  init    = 0.1
)

pp_check(Mustard.m1.brms, ndraws = 100)
summary(Mustard.m1.brms)

yrep <- posterior_predict(Mustard.m1.brms, ndraws = 500)
res  <- createDHARMa(
  simulatedResponse = t(yrep),
  observedResponse  = TargetData.Mustard$MustardProp_nz,
  integerResponse   = FALSE
)
plot(res)

p.Mustard_predict <- make_ce_plot(
  Mustard.m1.brms,
  title = "Mustard Canopy Gap by Treatment and Year",
  ylab  = "Mustard Canopy Gap (proportion)"
)
p.Mustard_predict
ggsave("Output/MustardCover_predict.png", p.Mustard_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(Mustard.m1.brms, "Output/mustard_cover_model.rds")
rm(Mustard.m1.brms, p.Mustard_predict, yrep, res)
gc()

# ============================================================
# 2. MUSTARD DENSITY (HIIN) - tweedie brms
# ============================================================
hist(TargetData$MustardDens_m2)

TargetData.MustardDens <- TargetData %>%
  filter(!is.na(MustardDens_m2))

cat("Mustard density zeros:",
    sum(TargetData.MustardDens$MustardDens_m2 == 0), "\n")

MustardDens.m1.brms <- brm(
  MustardDens_m2 ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData.MustardDens,
  family  = hurdle_gamma(link = "log"),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(3, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(MustardDens.m1.brms)

pp_check(MustardDens.m1.brms, ndraws = 100)
summary(MustardDens.m1.brms)

p.MustardDens_predict <- make_ce_plot(
  MustardDens.m1.brms,
  title   = "Mustard Density by Treatment and Year",
  ylab    = "Mustard Density (plants/m²)"#,
  #newdata = valid_newdata.MustardDens
)
p.MustardDens_predict
ggsave("Output/MustardDens_predict.png", p.MustardDens_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(MustardDens.m1.brms, "Output/mustard_dens_model.rds")
rm(MustardDens.m1.brms, p.MustardDens_predict)
gc()

# ============================================================
# 3. YELLOW STAR THISTLE PRESENCE (CESO) - binomial brms
# ============================================================
TargetData.YStar <- TargetData %>%
  filter(!is.na(YStarDens_m2)) %>%
  mutate(YStarPresence = as.integer(YStarDens_m2 > 0))

cat("YST presence table:\n")
print(table(TargetData.YStar$YStarPresence,
            TargetData.YStar$Year.f,
            TargetData.YStar$Treatment))

# No interaction — too sparse
YStarPresence.m1.brms <- brm(
  YStarPresence ~ Year.f + Treatment + (1|Block/Plot),
  data    = TargetData.YStar,
  family  = bernoulli(),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(0, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(YStarPresence.m1.brms, ndraws = 100)
summary(YStarPresence.m1.brms)

# Raw data summary plot (more informative than model predictions for sparse data)
YStar.summary <- TargetData.YStar %>%
  group_by(Year, Treatment) %>%
  summarise(
    n_present    = sum(YStarPresence, na.rm = TRUE),
    n_total      = n(),
    prop_present = mean(YStarPresence, na.rm = TRUE),
    .groups      = "drop"
  )

YStar.summary.mean <- YStar.summary %>%
  group_by(Year, Treatment) %>%
  summarise(mean_prop = mean(prop_present, na.rm = TRUE),
            .groups   = "drop")

YST.plot <- ggplot(YStar.summary,
                   aes(x = Year, y = prop_present,
                       color = Treatment, group = Treatment)) +
  geom_line(data = YStar.summary.mean,
            aes(x = Year, y = mean_prop,
                color = Treatment, group = Treatment),
            linewidth = 0.5, linetype = "dashed") +
  geom_jitter(width = 0.05, height = 0.01,
              aes(size = n_present)) +
  scale_size_continuous(name = "# plots with YST") +
  scale_color_manual(values = subtreatment_colors) +
  geom_vline(aes(xintercept = 2021.5), linetype = 4,
             color = "gray40") +
  theme_pinn +
  legend_bottom +
  ylab("Proportion of plots with YST present") +
  xlab("Year") +
  ggtitle("Yellow Star Thistle Presence by Treatment")

YST.plot
ggsave("Output/YST_presence.png", YST.plot,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(YStarPresence.m1.brms, "Output/ystar_presence_model.rds")
rm(YST.plot, YStarPresence.m1.brms)
gc()

# ============================================================
# 4. THATCH Depth  (cm) - hurdle_gamma brms
# ============================================================
hist(TargetData$AvThatch_cm)
cat("Thatch zeros:", sum(TargetData$AvThatch_cm == 0, na.rm = TRUE), "\n")

AvThatch_cm.m1.brms <- brm(
  AvThatch_cm ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData,
  family  = hurdle_gamma(link = "log"),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(1, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(AvThatch_cm.m1.brms, ndraws = 100)
summary(AvThatch_cm.m1.brms)

p.AvThatch_predict <- make_ce_plot(
  AvThatch_cm.m1.brms,
  title = "Thatch Depth by Treatment and Year",
  ylab  = "Thatch Depth (cm)"
)
p.AvThatch_predict
ggsave("Output/AvThatch_predict.png", p.AvThatch_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(AvThatch_cm.m1.brms, "Output/thatch_model.rds")
rm(AvThatch_cm.m1.brms, p.AvThatch_predict)
gc()

# ============================================================
# 5. VEGETATION HEIGHT (cm) - Gamma brms
# ============================================================
hist(TargetData$AvVegHeight_cm)

AvVegHeight.m1.brms <- brm(
  AvVegHeight_cm ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData,
  family  = Gamma(link = "log"),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(3, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(AvVegHeight.m1.brms, ndraws = 100)
summary(AvVegHeight.m1.brms)

p.AvVegHeight_predict <- make_ce_plot(
  AvVegHeight.m1.brms,
  title = "Vegetation Height by Treatment and Year",
  ylab  = "Mean Veg Height (cm)"
)
p.AvVegHeight_predict
ggsave("Output/AvVegHeight_predict.png", p.AvVegHeight_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(AvVegHeight.m1.brms, "Output/vegheight_model.rds")
rm(AvVegHeight.m1.brms, p.AvVegHeight_predict)
gc()

# ============================================================
# 6. VISUAL OBSTRUCTION - Gamma brms
# ============================================================
hist(TargetData$VisObs_Av)

VisObs_Av.m1.brms <- brm(
  VisObs_Av ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData,
  family  = Gamma(link = "log"),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(3, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(VisObs_Av.m1.brms, ndraws = 100)
summary(VisObs_Av.m1.brms)

p.VisObs_predict <- make_ce_plot(
  VisObs_Av.m1.brms,
  title = "Visual Obstruction by Treatment and Year",
  ylab  = "Visual Obstruction (cm)"
)
p.VisObs_predict
ggsave("Output/VisObs_predict.png", p.VisObs_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(VisObs_Av.m1.brms, "Output/visobs_model.rds")
rm(VisObs_Av.m1.brms, p.VisObs_predict)
gc()

# ============================================================
# 7. BARE GROUND - Beta brms
# ============================================================
hist(TargetData$BG_pct)
cat("BG zeros:", sum(TargetData$BGprop == 0, na.rm = TRUE), "\n")
cat("BG range:", range(TargetData$BGprop, na.rm = TRUE), "\n")

bareground.m1.brms <- brm(
  BGprop_nz ~ Year.f * Treatment + (1|Block) + (1|Year.f),
  data    = TargetData,
  family  = Beta(),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(-3, 1), class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = phi)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  control = list(adapt_delta   = 0.99,
                 max_treedepth = 15),
  seed    = brms_seed
)

pp_check(bareground.m1.brms, ndraws = 100)
summary(bareground.m1.brms)

p.bareground_predict <- make_ce_plot(
  bareground.m1.brms,
  title = "Bare Ground by Treatment and Year",
  ylab  = "Proportion Bare Ground"
)
p.bareground_predict
ggsave("Output/Bareground_predict.png", p.bareground_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(bareground.m1.brms, "Output/bareground_model.rds")
rm(bareground.m1.brms, p.bareground_predict)
gc()

# ============================================================
# 8. YIELD (lbs/ac) - Gamma brms
# ============================================================
hist(TargetData$AllYieldEstEst_lbsAc)

AllYield.m1.brms <- brm(
  AllYieldEstEst_lbsAc ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData,
  family  = Gamma(link = "log"),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(7, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(AllYield.m1.brms, ndraws = 100)
summary(AllYield.m1.brms)

p.AllYield_predict <- make_ce_plot(
  AllYield.m1.brms,
  title = "Estimated Yield by Treatment and Year",
  ylab  = "Yield (lbs/ac)"
)
p.AllYield_predict
ggsave("Output/AllYield_predict.png", p.AllYield_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(AllYield.m1.brms, "Output/yield_model.rds")
rm(AllYield.m1.brms, p.AllYield_predict)
gc()

# ============================================================
# 9. THATCH BIOMASS (lbs/ac) - negbinomial brms
# ============================================================
hist(TargetData$Thatch_lbsAc)

thatch.m1.brms <- brm(
  Thatch_lbsAc ~ Year.f * Treatment + (1|Block/Plot),
  data    = TargetData,
  family  = negbinomial(),
  prior   = c(
    prior(normal(0, 1),  class = b),
    prior(normal(8, 1),  class = Intercept),
    prior(normal(0, 1),  class = sd),
    prior(gamma(2, 0.5), class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  control = list(adapt_delta = 0.95),
  seed    = brms_seed
)

pp_check(thatch.m1.brms, ndraws = 100) + ggtitle("Thatch")
summary(thatch.m1.brms)

p.thatch_predict <- make_ce_plot(
  thatch.m1.brms,
  title = "Thatch Biomass by Treatment and Year",
  ylab  = "Thatch (lbs/ac)"
)
p.thatch_predict
ggsave("Output/Thatch_biomass_predict.png", p.thatch_predict,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(thatch.m1.brms, "Output/thatch_biomass_model.rds")
rm(thatch.m1.brms, p.thatch_predict)
gc()


##pp plots


p.a <- pp_check(Mustard.m1.brms, ndraws = 100) + ggtitle("Mustard Canopy Gap")
p.b <- pp_check(MustardDens.m1.brms, ndraws = 100) + ggtitle("Mustard Density")
p.c <- pp_check(YStarPresence.m1.brms, ndraws = 100)+ ggtitle("YST")
p.d <- pp_check(AvVegHeight.m1.brms, ndraws = 100)+ ggtitle("Veg Height")
p.e <- pp_check(VisObs_Av.m1.brms, ndraws = 100) + ggtitle("Vis Obstruction")
p.f <- pp_check(bareground.m1.brms, ndraws = 100) + ggtitle("Bare Ground")
p.g <- pp_check(AllYield.m1.brms, ndraws = 100) + ggtitle("Yield")
p.h <- pp_check(thatch.m1.brms, ndraws = 100) + ggtitle("Thatch")


combined_plot_pp<-
  (p.a     | p.b)      /
  (p.c         | p.d)     /
  (p.e         | p.f)    /
  (p.g         | p.h)    +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "pp plots",
    theme    = theme(
      plot.title    = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  ) &
  theme_classic(base_size = 14) 


# ============================================================
# RELOAD SAVED MODELS
# ============================================================
# AvThatch_cm.m1.brms    <- readRDS("Output/thatch_model.rds")
# VisObs_Av.m1.brms      <- readRDS("Output/visobs_model.rds")
# bareground.m1.brms     <- readRDS("Output/bareground_model.rds")
# AllYield.m1.brms       <- readRDS("Output/yield_model.rds")
# thatch.m1.brms         <- readRDS("Output/thatch_biomass_model.rds")
# Mustard.m1.brms        <- readRDS("Output/mustard_cover_model.rds")
# MustardDens.m1.brms    <- readRDS("Output/mustard_dens_model.rds")
# AvVegHeight.m1.brms    <- readRDS("Output/vegheight_model.rds")
# YStarPresence.m1.brms  <- readRDS("Output/ystar_presence_model.rds")

# ============================================================
# COMBINED EFFECTS PLOT
# ============================================================
AvThatch_cm.m1.brms  <- readRDS("Output/thatch_model.rds")
VisObs_Av.m1.brms    <- readRDS("Output/visobs_model.rds")
bareground.m1.brms   <- readRDS("Output/bareground_model.rds")
AllYield.m1.brms     <- readRDS("Output/yield_model.rds")
thatch.m1.brms       <- readRDS("Output/thatch_biomass_model.rds")
Mustard.m1.brms      <- readRDS("Output/mustard_cover_model.rds")
MustardDens.m1.brms  <- readRDS("Output/mustard_dens_model.rds")
AvVegHeight.m1.brms  <- readRDS("Output/vegheight_model.rds")
YStarPresence.m1.brms  <- readRDS("Output/ystar_presence_model.rds")

panel_thatch <- make_ce_plot(AvThatch_cm.m1.brms,
                             title = "Thatch Depth (cm)",
                             ylab  = "Thatch Depth (cm)")

panel_visobs <- make_ce_plot(VisObs_Av.m1.brms,
                             title = "Visual Obstruction (cm)",
                             ylab  = "Visual Obstruction (cm)")

panel_bg     <- make_ce_plot(bareground.m1.brms,
                             title = "Bare Ground (proportion)",
                             ylab  = "Bare Ground (proportion)")

panel_yield  <- make_ce_plot(AllYield.m1.brms,
                             title = "Yield (lbs/ac)",
                             ylab  = "Yield (lbs/ac)")

panel_thatch_bio <- make_ce_plot(thatch.m1.brms,
                                 title = "Thatch Biomass (lbs/ac)",
                                 ylab  = "Thatch (lbs/ac)")

panel_mustard <- make_ce_plot(Mustard.m1.brms,
                              title   = "Mustard Canopy Gap",
                              ylab    = "Mustard Canopy Gap")#,
                              #newdata = valid_newdata.Mustard)

panel_mustarddens <- make_ce_plot(MustardDens.m1.brms,
                                  title   = "Mustard Density (plants/m²)",
                                  ylab    = "Mustard Density (plants/m²)")#,
                                  #newdata = valid_newdata.MustardDens)

panel_vegheight <- make_ce_plot(AvVegHeight.m1.brms,
                                title = "Vegetation Height (cm)",
                                ylab  = "Veg Height (cm)")

YStarPresence <- make_ce_plot(YStarPresence.m1.brms,
                              title = "YST Presence",
                              ylab  = "YST Presence")


combined_plot_A<-
  (panel_thatch     | panel_visobs)      /
  (panel_bg         | panel_yield)       +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Treatment Effects on Habitat Variables",
    subtitle = "Predicted means with 95% credible intervals",
    theme    = theme(
      plot.title    = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  ) &
  theme_classic(base_size = 14) +   
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 7))

combined_plot_A
ggsave("Output/Combined_effects_A.png", combined_plot_A,
       width = 30, height = 20, units = "cm", dpi = 300)




combined_plot_B <-
  (panel_vegheight  | panel_mustard)     /
  (panel_thatch_bio | panel_mustarddens) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Treatment Effects on Habitat Variables",
    subtitle = "Predicted means with 95% credible intervals",
    theme    = theme(
      plot.title    = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  ) &
  theme_classic(base_size = 14) +        # <-- sets all font sizes proportionally
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11)
  )



combined_plot_B
ggsave("Output/Combined_effects_B.png", combined_plot_B,
       width = 30, height = 20, units = "cm", dpi = 300)







rm(AvThatch_cm.m1.brms, VisObs_Av.m1.brms, bareground.m1.brms,
   AllYield.m1.brms, thatch.m1.brms, Mustard.m1.brms,
   MustardDens.m1.brms, AvVegHeight.m1.brms,
   panel_thatch, panel_visobs, panel_bg, panel_yield,
   panel_vegheight, panel_mustard, panel_mustarddens,
   panel_thatch_bio, combined_plot)
gc()