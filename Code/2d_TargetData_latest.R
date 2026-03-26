# ============================================================
# TARGET FINE SCALE DATA - MODELS AND PLOTS
# ============================================================

library(tidyverse)
library(lme4)
library(readxl)
library(MASS)
library(sjPlot)
library(marginaleffects)
library(paletteer)
library(DHARMa)
library(ggeffects)
library(brms)
library(cmdstanr)
library(glmmTMB)
library(patchwork)

dev.off()

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
    Thatch_lbsAc = as.integer(Thatch_lbsAc)
  )

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
subtrt_colors <- c(
  "Control"                  = "#8B0000",
  "Scraped + Seeded"         = "#4A4A4A",
  "Mowed"                    = "#E69A00",
  "Mowed + Scraped + Seeded" = "#4D7A3A"
)

plot_colors <- c(
  "Ungrazed" = "#2166AC",
  "Grazed"   = "#D6604D",
  "Seasonal" = "#4DAC26"
)

# Shared brms settings
brms_cores   <- 4
brms_threads <- threading(16)
brms_backend <- "cmdstanr"
brms_seed    <- 123

# Shared plot theme
theme_pinn <- theme_gray(base_size = 14)

# ============================================================
# BUILD VALID NEWDATA GRIDS (only observed combinations)
# ============================================================

# Check observed Plot x Subplot combinations
observed_combinations <- TargetData %>%
  dplyr::select(Plot, Subplot) %>%
  distinct() %>%
  arrange(Plot, Subplot)
cat("Observed Plot x Subplot combinations:\n")
print(observed_combinations)

# Full grid for main TargetData models
# Year.f x Plot x Subplot — only observed Plot/Subplot combos
valid_newdata <- TargetData %>%
  dplyr::select(Year.f, Plot, Subplot, Block, PPT_CM,
                BGprop_nz) %>%
  distinct(Year.f, Plot, Subplot, .keep_all = TRUE) %>%
  arrange(Year.f, Plot, Subplot)

cat("\nValid newdata grid dimensions:", nrow(valid_newdata), "rows\n")
print(valid_newdata %>% dplyr::select(Year.f, Plot, Subplot), n = 50)

# Grid for Mustard model (subset of years — no 2021)
valid_newdata.Mustard <- TargetData %>%
  filter(!is.na(MustardCovr_pct)) %>%
  mutate(
    MustardCovr_pct = as.integer(MustardCovr_pct),
    MustardProp     = MustardCovr_pct / 100,
    MustardProp_nz  = ifelse(MustardProp == 0, 0.0001, MustardProp)
  ) %>%
  distinct(Year.f, Plot, Subplot, .keep_all = TRUE) %>%
  arrange(Year.f, Plot, Subplot)

# Grid for MustardDens model
valid_newdata.MustardDens <- TargetData %>%
  filter(!is.na(MustardDens_m2)) %>%
  distinct(Year.f, Plot, Subplot, .keep_all = TRUE) %>%
  arrange(Year.f, Plot, Subplot)

# ============================================================
# QUICK OVERVIEW PLOT
# ============================================================
TargetData_long <- TargetData %>%
  dplyr::select(Year, Year.f, Block, Plot, Subplot, Plot_SubPlot,
                VisObs_Av, AllYieldEstEst_lbsAc, Thatch_lbsAc,
                AvVegHeight_cm, AvThatch_cm, BG_pct,
                MustardCovr_pct, MustardDens_m2, PoppyDens_m2,
                YStarDens_m2, GoldfieldDens_m2, OwlsClvDens_m2,
                TarweedDens_m2) %>%
  pivot_longer(
    !c(Year, Year.f, Block, Plot, Subplot, Plot_SubPlot),
    names_to  = "Variable",
    values_to = "Value"
  )

ggplot(TargetData_long, aes(x = Year.f, y = Value, color = Plot_SubPlot)) +
  geom_boxplot() +
  facet_wrap(. ~ Variable, scales = "free_y")

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# brms: conditional_effects restricted to valid combinations
make_ce_plot <- function(model, title, ylab,
                         newdata      = valid_newdata,
                         color_values = subtrt_colors,
                         ylim_vals    = NULL) {
  ce <- conditional_effects(
    model,
    effects    = "Year.f:Subplot",
    conditions = data.frame(Plot = levels(TargetData$Plot)),
    newdata    = newdata,
    re_formula = NA  # population-level predictions
  )
  p <- plot(ce, plot = FALSE)[[1]] +
    theme_pinn +
    ylab(ylab) +
    xlab("Year") +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values  = color_values) +
    facet_wrap(~Plot) +
    ggtitle(title)
  if (!is.null(ylim_vals)) p <- p + ylim(ylim_vals)
  p
}

# glmer/glmmTMB: ggpredict restricted to valid combinations
make_ggpredict_plot <- function(model, title, ylab,
                                newdata      = valid_newdata,
                                color_values = subtrt_colors,
                                ylim_vals    = NULL,
                                pred_type    = "fixed") {
  p <- ggpredict(model,
                 terms   = c("Year.f", "Subplot", "Plot"),
                 type    = pred_type,
                 newdata = newdata) |>
    plot() +
    theme_pinn +
    ylab(ylab) +
    xlab("Year") +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values  = color_values) +
    facet_wrap(~facet) +
    ggtitle(title)
  if (!is.null(ylim_vals)) p <- p + ylim(ylim_vals)
  p
}

# ============================================================
# 1. MUSTARD COVER (HIIN) - ZIBeta brms
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
  MustardProp_nz ~ Year.f * Subplot + Plot + (1|Block/Plot),
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
  title   = "Mustard Cover by Treatment and Year",
  ylab    = "Mustard Cover (proportion)",
  newdata = valid_newdata.Mustard
)
p.Mustard_predict
ggsave("Output/MustardCover_predict.png", p.Mustard_predict,
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(Mustard.m1.brms, "Output/mustard_cover_model.rds")
rm(Mustard.m1.brms, p.Mustard_predict, yrep, res)
gc()

# ============================================================
# 2. MUSTARD DENSITY (HIIN) - Tweedie glmmTMB
# ============================================================
hist(TargetData$MustardDens_m2)

TargetData.MustardDens <- TargetData %>%
  filter(!is.na(MustardDens_m2))

cat("Mustard density zeros:", sum(TargetData.MustardDens$MustardDens_m2 == 0), "\n")

MustardDens.m1 <- glmmTMB(
  MustardDens_m2 ~ Year.f * Subplot + Plot + (1|Block/Plot),
  family = tweedie(link = "log"),
  data   = TargetData.MustardDens
)
summary(MustardDens.m1)
simulateResiduals(MustardDens.m1, plot = TRUE)

p.MustardDens_predict <- make_ggpredict_plot(
  MustardDens.m1,
  title   = "Mustard Density by Treatment and Year",
  ylab    = "Mustard Density (plants/m²)",
  newdata = valid_newdata.MustardDens
)
p.MustardDens_predict
ggsave("Output/MustardDens_predict.png", p.MustardDens_predict,
       width = 35, height = 20, units = "cm", dpi = 300)
rm(p.MustardDens_predict)
gc()

# ============================================================
# 3. YELLOW STAR THISTLE PRESENCE (CESO)
# ============================================================
TargetData.YStar <- TargetData %>%
  filter(!is.na(YStarDens_m2)) %>%
  mutate(YStarPresence = as.integer(YStarDens_m2 > 0))

cat("YST presence table:\n")
print(table(TargetData.YStar$YStarPresence,
            TargetData.YStar$Year.f,
            TargetData.YStar$Subplot))

# No Year*Subplot interaction — too sparse
YStarPresence.m1 <- glmer(
  YStarPresence ~ Year.f + Subplot + (1|Block/Plot),
  family = binomial,
  data   = TargetData.YStar
)
summary(YStarPresence.m1)
simulateResiduals(YStarPresence.m1, plot = TRUE)

YStar.summary <- TargetData.YStar %>%
  group_by(Year, Plot, Subplot) %>%
  summarise(
    n_present    = sum(YStarPresence, na.rm = TRUE),
    n_total      = n(),
    prop_present = mean(YStarPresence, na.rm = TRUE),
    .groups = "drop"
  )

YStar.summary.mean <- YStar.summary %>%
  group_by(Year, Subplot) %>%
  summarise(mean_prop = mean(prop_present, na.rm = TRUE),
            .groups = "drop")

YST.plot <- ggplot(YStar.summary,
                   aes(x = Year, y = prop_present,
                       color = Subplot, group = Subplot)) +
  geom_line(data = YStar.summary.mean,
            aes(x = Year, y = mean_prop,
                color = Subplot, group = Subplot),
            linewidth = 0.5, linetype = "dashed") +
  geom_jitter(width = 0.05, height = 0.01, aes(size = n_present)) +
  scale_size_continuous(name = "# plots with YST") +
  scale_color_manual(values = subtrt_colors) +
  geom_vline(aes(xintercept = 2021.5), linetype = 4) +
  theme_pinn +
  ylab("Proportion of plots with YST present") +
  xlab("Year") +
  ggtitle("Yellow Star Thistle Presence by Treatment")

YST.plot
ggsave("Output/YST_presence.png", YST.plot,
       width = 25, height = 15, units = "cm", dpi = 300)
rm(YST.plot)
gc()

# ============================================================
# 4. THATCH HEIGHT (cm) - hurdle_gamma brms
# ============================================================
hist(TargetData$AvThatch_cm)
cat("Thatch zeros:", sum(TargetData$AvThatch_cm == 0, na.rm = TRUE), "\n")

AvThatch_cm.m1.brms <- brm(
  AvThatch_cm ~ Year.f * Subplot + Plot + (1|Block/Plot),
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
  title = "Thatch Height by Treatment and Year",
  ylab  = "Thatch Height (cm)"
)
p.AvThatch_predict
ggsave("Output/AvThatch_predict.png", p.AvThatch_predict,
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(AvThatch_cm.m1.brms, "Output/thatch_model.rds")
rm(AvThatch_cm.m1.brms, p.AvThatch_predict)
gc()

# ============================================================
# 5. VEGETATION HEIGHT (cm) - Gamma glmer
# ============================================================
hist(TargetData$AvVegHeight_cm)

AvVegHeight.m1 <- glmer(
  AvVegHeight_cm ~ Year.f * Subplot + Plot + (1|Block/Plot),
  family = Gamma(link = "log"),
  nAGQ   = 0,
  data   = TargetData
)
summary(AvVegHeight.m1)
simulateResiduals(AvVegHeight.m1, plot = TRUE)

p.AvVegHeight_predict <- make_ggpredict_plot(
  AvVegHeight.m1,
  title     = "Vegetation Height by Treatment and Year",
  ylab      = "Mean Veg Height (cm)",
  pred_type = "fixed"
)
p.AvVegHeight_predict
ggsave("Output/AvVegHeight_predict.png", p.AvVegHeight_predict,
       width = 35, height = 20, units = "cm", dpi = 300)
rm(p.AvVegHeight_predict)
gc()

# ============================================================
# 6. VISUAL OBSTRUCTION - Gamma brms
# ============================================================
hist(TargetData$VisObs_Av)

VisObs_Av.m1.brms <- brm(
  VisObs_Av ~ Year.f * Subplot + Plot + (1|Block/Plot),
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
       width = 35, height = 20, units = "cm", dpi = 300)

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
  BGprop_nz ~ Year.f * Subplot + Plot + (1|Block) + (1|Year.f),
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
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(bareground.m1.brms, "Output/bareground_model.rds")
rm(bareground.m1.brms, p.bareground_predict)
gc()

# ============================================================
# 8. YIELD (lbs/ac) - Gamma brms
# ============================================================
hist(TargetData$AllYieldEstEst_lbsAc)

AllYield.m1.brms <- brm(
  AllYieldEstEst_lbsAc ~ Year.f * Subplot + Plot + (1|Block/Plot),
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
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(AllYield.m1.brms, "Output/yield_model.rds")
rm(AllYield.m1.brms, p.AllYield_predict)
gc()

# ============================================================
# 9. THATCH BIOMASS (lbs/ac) - negbinomial brms
# ============================================================
hist(TargetData$Thatch_lbsAc)

thatch.m1.brms <- brm(
  Thatch_lbsAc ~ Year.f * Subplot + Plot + (1|Block/Plot),
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

pp_check(thatch.m1.brms, ndraws = 100)
summary(thatch.m1.brms)

p.thatch_predict <- make_ce_plot(
  thatch.m1.brms,
  title = "Thatch Biomass by Treatment and Year",
  ylab  = "Thatch (lbs/ac)"
)
p.thatch_predict
ggsave("Output/Thatch_biomass_predict.png", p.thatch_predict,
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(thatch.m1.brms, "Output/thatch_biomass_model.rds")
rm(thatch.m1.brms, p.thatch_predict)
gc()

# ============================================================
# RELOAD SAVED MODELS
# ============================================================
# AvThatch_cm.m1.brms  <- readRDS("Output/thatch_model.rds")
# VisObs_Av.m1.brms    <- readRDS("Output/visobs_model.rds")
# bareground.m1.brms   <- readRDS("Output/bareground_model.rds")
# AllYield.m1.brms     <- readRDS("Output/yield_model.rds")
# thatch.m1.brms       <- readRDS("Output/thatch_biomass_model.rds")
# Mustard.m1.brms      <- readRDS("Output/mustard_cover_model.rds")

# ============================================================
# COMBINED EFFECTS PLOT
# ============================================================

# Reload all models
AvThatch_cm.m1.brms <- readRDS("Output/thatch_model.rds")
VisObs_Av.m1.brms   <- readRDS("Output/visobs_model.rds")
bareground.m1.brms  <- readRDS("Output/bareground_model.rds")
AllYield.m1.brms    <- readRDS("Output/yield_model.rds")
thatch.m1.brms      <- readRDS("Output/thatch_biomass_model.rds")
Mustard.m1.brms     <- readRDS("Output/mustard_cover_model.rds")

# Build panels — all restricted to valid combinations
panel_thatch <- make_ce_plot(AvThatch_cm.m1.brms,
                             title = "Thatch Height (cm)",
                             ylab  = "Thatch Height (cm)")

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
                              title   = "Mustard Cover (proportion)",
                              ylab    = "Mustard Cover (proportion)",
                              newdata = valid_newdata.Mustard)

panel_vegheight <- make_ggpredict_plot(
  AvVegHeight.m1,
  title     = "Vegetation Height (cm)",
  ylab      = "Veg Height (cm)",
  pred_type = "fixed"
)

panel_mustarddens <- make_ggpredict_plot(
  MustardDens.m1,
  title   = "Mustard Density (plants/m²)",
  ylab    = "Mustard Density (plants/m²)",
  newdata = valid_newdata.MustardDens
)

# Combine with patchwork
combined_plot <-
  (panel_thatch     | panel_visobs)      /
  (panel_bg         | panel_yield)       /
  (panel_vegheight  | panel_mustard)     /
  (panel_thatch_bio | panel_mustarddens) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Treatment Effects on Habitat Variables",
    subtitle = "Predicted means with 95% credible/confidence intervals",
    theme    = theme(
      plot.title    = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  ) &
  theme(legend.position = "bottom")

combined_plot
ggsave("Output/Combined_effects.png", combined_plot,
       width = 80, height = 80, units = "cm", dpi = 200)

# Clean up
rm(AvThatch_cm.m1.brms, VisObs_Av.m1.brms, bareground.m1.brms,
   AllYield.m1.brms, thatch.m1.brms, Mustard.m1.brms,
   panel_thatch, panel_visobs, panel_bg, panel_yield,
   panel_vegheight, panel_mustard, panel_mustarddens,
   panel_thatch_bio, combined_plot)
gc()


#### subplot effects below




# ============================================================
# TARGET FINE SCALE DATA - MODELS AND PLOTS
# ============================================================

library(tidyverse)
library(lme4)
library(readxl)
library(MASS)
library(sjPlot)
library(marginaleffects)
library(paletteer)
library(DHARMa)
library(ggeffects)
library(brms)
library(cmdstanr)
library(glmmTMB)
library(patchwork)

dev.off()

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
    Thatch_lbsAc = as.integer(Thatch_lbsAc)
  )

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
    Rain.f = factor(Rain.f, levels = c("Low", "Med", "High")),
    BGprop    = BG_pct / 100,
    BGprop_nz = ifelse(BGprop == 0, 0.0001, BGprop)
  )

# ============================================================
# COLOR PALETTES AND SHARED SETTINGS
# ============================================================
subtrt_colors <- c(
  "Control"                  = "#8B0000",
  "Scraped + Seeded"         = "#4A4A4A",
  "Mowed"                    = "#E69A00",
  "Mowed + Scraped + Seeded" = "#4D7A3A"
)

# Shared brms settings
brms_cores   <- 4
brms_threads <- threading(16)
brms_backend <- "cmdstanr"
brms_seed    <- 123

# Shared conditional effects conditions
plot_conditions <- data.frame(Plot = levels(TargetData$Plot))

# Shared plot theme
theme_pinn <- theme_gray(base_size = 14)

# ============================================================
# QUICK OVERVIEW PLOT
# ============================================================
TargetData_long <- TargetData %>%
  dplyr::select(Year, Year.f, Block, Plot, Subplot, Plot_SubPlot,
                VisObs_Av, AllYieldEstEst_lbsAc, Thatch_lbsAc,
                AvVegHeight_cm, AvThatch_cm, BG_pct,
                MustardCovr_pct, MustardDens_m2, PoppyDens_m2,
                YStarDens_m2, GoldfieldDens_m2, OwlsClvDens_m2,
                TarweedDens_m2) %>%
  pivot_longer(
    !c(Year, Year.f, Block, Plot, Subplot, Plot_SubPlot),
    names_to  = "Variable",
    values_to = "Value"
  )

ggplot(TargetData_long, aes(x = Year.f, y = Value, color = Plot_SubPlot)) +
  geom_boxplot() +
  facet_wrap(. ~ Variable, scales = "free_y")

# ============================================================
# HELPER FUNCTION: standard effects plot from conditional_effects
# ============================================================
make_ce_plot <- function(model, title, ylab,
                         color_values = subtrt_colors,
                         ylim_vals    = NULL) {
  ce <- conditional_effects(model,
                            effects    = "Year.f:Subplot",
                            conditions = plot_conditions)
  p <- plot(ce, plot = FALSE)[[1]] +
    theme_pinn +
    ylab(ylab) +
    xlab("Year") +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values  = color_values) +
    facet_wrap(~Plot) +
    ggtitle(title)
  if (!is.null(ylim_vals)) p <- p + ylim(ylim_vals)
  p
}

# ============================================================
# 1. MUSTARD COVER (HIIN) - ZIBeta model
# ============================================================
TargetData.Mustard <- TargetData.Mustard %>%
  mutate(MustardProp_nz = ifelse(MustardProp == 0, 0.0001, MustardProp))
# no need to handle ones since there are none

Mustard.m1.brms <- brm(
  MustardProp_nz ~ Year.f * Subplot + Plot + PPT_CM + (1|Block/Plot),
  data    = TargetData.Mustard,
  family  = zero_inflated_beta(),
  prior   = c(
    prior(normal(0, 1),   class = b),
    prior(normal(-2, 1),  class = Intercept),
    prior(normal(0, 1),   class = sd),
    prior(exponential(1), class = phi)  # exponential keeps phi strictly positive
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed,
  init    = 0.1  # start chains closer to zero to avoid boundary issues
)

pp_check(Mustard.m1.brms, ndraws = 100)
summary(Mustard.m1.brms)

# DHARMa check
yrep <- posterior_predict(Mustard.m1.brms, ndraws = 500)
res  <- createDHARMa(
  simulatedResponse = t(yrep),
  observedResponse  = TargetData.Mustard$MustardProp_nz,
  integerResponse   = FALSE
)
plot(res)

# Save effects plot
ce_mustard <- conditional_effects(Mustard.m1.brms,
                                  effects    = "Year.f:Subplot",
                                  conditions = plot_conditions)

p.Mustard_predict <- plot(ce_mustard, plot = FALSE)[[1]] +
  theme_pinn +
  ylab("Mustard Cover (proportion)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~Plot) +
  ggtitle("Mustard Cover by Treatment and Year")

p.Mustard_predict
ggsave("Output/MustardCover_predict.png", p.Mustard_predict,
       width = 35, height = 20, units = "cm", dpi = 300)

#plot checking
TargetData.Mustard %>%
  filter(Plot == "Ungrazed") %>%
  group_by(Year.f, Subplot) %>%
  summarise(
    n          = n(),
    n_zeros    = sum(MustardProp == 0),
    mean_cover = mean(MustardProp),
    max_cover  = max(MustardProp),
    .groups = "drop"
  ) %>%
  print(n = 30)

saveRDS(Mustard.m1.brms, "Output/mustard_cover_model.rds")
rm(Mustard.m1.brms, p.Mustard_predict, ce_mustard)
gc()

# ============================================================
# 2. MUSTARD DENSITY (HIIN) - Tweedie glmmTMB
# ============================================================
hist(TargetData$MustardDens_m2)

TargetData.MustardDens <- TargetData %>%
  filter(!is.na(MustardDens_m2))

cat("Mustard density zeros:", sum(TargetData.MustardDens$MustardDens_m2 == 0), "\n")

MustardDens.m1 <- glmmTMB(
  MustardDens_m2 ~ Year.f * Subplot + Plot + (1|Block/Plot),
  family = tweedie(link = "log"),
  data   = TargetData.MustardDens
)
summary(MustardDens.m1)
simulateResiduals(MustardDens.m1, plot = TRUE)

p.MustardDens_predict <- ggpredict(MustardDens.m1,
                                   terms = c("Year.f", "Subplot", "Plot"),
                                   type  = "fixed") |>
  plot() +
  theme_pinn +
  ylab("Mustard Density (plants/m²)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~facet) +
  ggtitle("Mustard Density by Treatment and Year")

p.MustardDens_predict
ggsave("Output/MustardDens_predict.png", p.MustardDens_predict,
       width = 35, height = 20, units = "cm", dpi = 300)
rm(p.MustardDens_predict)
gc()

# ============================================================
# 3. YELLOW STAR THISTLE PRESENCE (CESO)
# ============================================================
TargetData.YStar <- TargetData %>%
  filter(!is.na(YStarDens_m2)) %>%
  mutate(YStarPresence = as.integer(YStarDens_m2 > 0))

cat("YST presence table:\n")
print(table(TargetData.YStar$YStarPresence,
            TargetData.YStar$Year.f,
            TargetData.YStar$Subplot))

YStarPresence.m1 <- glmer(
  YStarPresence ~ Year.f + Subplot + (1|Block/Plot),
  family = binomial,
  data   = TargetData.YStar
)
summary(YStarPresence.m1)
simulateResiduals(YStarPresence.m1, plot = TRUE)

# Note: no Year*Subplot interaction for YST — too sparse (see table above)

YStar.summary <- TargetData.YStar %>%
  group_by(Year, Plot, Subplot) %>%
  summarise(
    n_present    = sum(YStarPresence, na.rm = TRUE),
    n_total      = n(),
    prop_present = mean(YStarPresence, na.rm = TRUE),
    .groups = "drop"
  )

YStar.summary.mean <- YStar.summary %>%
  group_by(Year, Subplot) %>%
  summarise(mean_prop = mean(prop_present, na.rm = TRUE),
            .groups = "drop")

YST.plot <- ggplot(YStar.summary,
                   aes(x = Year, y = prop_present,
                       color = Subplot, group = Subplot)) +
  geom_line(data = YStar.summary.mean,
            aes(x = Year, y = mean_prop, color = Subplot, group = Subplot),
            linewidth = 0.5, linetype = "dashed") +
  geom_jitter(width = 0.05, height = 0.01, aes(size = n_present)) +
  scale_size_continuous(name = "# plots with YST") +
  scale_color_manual(values = subtrt_colors) +
  geom_vline(aes(xintercept = 2021.5), linetype = 4) +
  theme_pinn +
  ylab("Proportion of plots with YST present") +
  xlab("Year") +
  ggtitle("Yellow Star Thistle Presence by Treatment")

YST.plot
ggsave("Output/YST_presence.png", YST.plot,
       width = 25, height = 15, units = "cm", dpi = 300)
rm(YST.plot)
gc()

# ============================================================
# 4. THATCH HEIGHT (cm) - hurdle_gamma brms + Year*Subplot
# ============================================================
hist(TargetData$AvThatch_cm)
cat("Thatch zeros:", sum(TargetData$AvThatch_cm == 0, na.rm = TRUE), "\n")

AvThatch_cm.m1.brms <- brm(
  AvThatch_cm ~ Year.f * Subplot + Plot + PPT_CM + (1|Block/Plot),
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
  title = "Thatch Height by Treatment and Year",
  ylab  = "Thatch Height (cm)"
)
p.AvThatch_predict
ggsave("Output/AvThatch_predict.png", p.AvThatch_predict,
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(AvThatch_cm.m1.brms, "Output/thatch_model.rds")
rm(AvThatch_cm.m1.brms)
gc()

# ============================================================
# 5. VEGETATION HEIGHT (cm) - Gamma glmer + Year*Subplot
# ============================================================
hist(TargetData$AvVegHeight_cm)

AvVegHeight.m1 <- glmer(
  AvVegHeight_cm ~ Year.f * Subplot + Plot + (1|Block/Plot),
  family = Gamma(link = "log"),
  nAGQ   = 0,
  data   = TargetData
)
summary(AvVegHeight.m1)
simulateResiduals(AvVegHeight.m1, plot = TRUE)

p.AvVegHeight_predict <- ggpredict(AvVegHeight.m1,
                                   terms = c("Year.f", "Subplot", "Plot"),
                                   type  = "random") |>  # include random effects
  plot() +
  theme_pinn +
  ylab("Mean Veg Height (cm)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~facet) +
  ggtitle("Vegetation Height by Treatment and Year")

p.AvVegHeight_predict
ggsave("Output/AvVegHeight_predict.png", p.AvVegHeight_predict,
       width = 35, height = 20, units = "cm", dpi = 300)
rm(p.AvVegHeight_predict)
gc()

# ============================================================
# 6. VISUAL OBSTRUCTION - Gamma brms + Year*Subplot
# ============================================================
hist(TargetData$VisObs_Av)

VisObs_Av.m1.brms <- brm(
  VisObs_Av ~ Year.f * Subplot + Plot + PPT_CM + (1|Block/Plot),
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
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(VisObs_Av.m1.brms, "Output/visobs_model.rds")
rm(VisObs_Av.m1.brms)
gc()

# ============================================================
# 7. BARE GROUND - Beta brms + Year*Subplot
# ============================================================
hist(TargetData$BG_pct)
cat("BG zeros:", sum(TargetData$BGprop == 0, na.rm = TRUE), "\n")
cat("BG range:", range(TargetData$BGprop, na.rm = TRUE), "\n")

bareground.m1.brms <- brm(
  BGprop_nz ~ Year.f * Subplot + Plot + PPT_CM + (1|Block) + (1|Year.f),
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
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(bareground.m1.brms, "Output/bareground_model.rds")
rm(bareground.m1.brms)
gc()

# ============================================================
# 8. YIELD (lbs/ac) - Gamma brms + Year*Subplot
# ============================================================
hist(TargetData$AllYieldEstEst_lbsAc)

AllYield.m1.brms <- brm(
  AllYieldEstEst_lbsAc ~ Year.f * Subplot + Plot + PPT_CM + (1|Block/Plot),
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
       width = 35, height = 20, units = "cm", dpi = 300)

# PPT effect
ce_ppt <- conditional_effects(AllYield.m1.brms, effects = "PPT_CM")
p.AllYield_ppt <- plot(ce_ppt, plot = FALSE)[[1]] +
  theme_pinn +
  ylab("Yield (lbs/ac)") +
  xlab("Precipitation (cm)") +
  ggtitle("Effect of Rainfall on Yield")

p.AllYield_ppt
ggsave("Output/AllYield_ppt.png", p.AllYield_ppt,
       width = 20, height = 15, units = "cm", dpi = 300)

saveRDS(AllYield.m1.brms, "Output/yield_model.rds")
rm(AllYield.m1.brms, ce_ppt, p.AllYield_ppt)
gc()

# ============================================================
# 9. THATCH BIOMASS (lbs/ac) - negative binomial glmer
# ============================================================
thatch.m1.brms <- brm(
  Thatch_lbsAc ~ Year.f * Subplot + Plot + (1|Block/Plot),
  data    = TargetData,
  family  = negbinomial(),  # estimates shape freely
  prior   = c(
    prior(normal(0, 1),   class = b),
    prior(normal(8, 1),   class = Intercept),  # log(3000) ≈ 8
    prior(normal(0, 1),   class = sd),
    prior(gamma(2, 0.5),  class = shape)
  ),
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = brms_cores,
  threads = brms_threads,
  backend = brms_backend,
  seed    = brms_seed
)

pp_check(thatch.m1.brms, ndraws = 100)
summary(thatch.m1.brms)

p.thatch_predict <- make_ce_plot(
  thatch.m1.brms,
  title = "Thatch Biomass by Treatment and Year",
  ylab  = "Thatch (lbs/ac)"
)


p.thatch_predict
ggsave("Output/Thatch_biomass_predict.png", p.thatch_predict,
       width = 35, height = 20, units = "cm", dpi = 300)

saveRDS(thatch.m1.brms, "Output/thatch_biomass_model.rds")
rm(thatch.m1.brms, p.thatch_predict)
gc()


# ============================================================
# RELOAD SAVED MODELS (run this block in future sessions)
# ============================================================
# AvThatch_cm.m1.brms  <- readRDS("Output/thatch_model.rds")
# VisObs_Av.m1.brms    <- readRDS("Output/visobs_model.rds")
# bareground.m1.brms   <- readRDS("Output/bareground_model.rds")
# AllYield.m1.brms     <- readRDS("Output/yield_model.rds")

# ============================================================
# COMBINED EFFECTS PLOT (all variables on one page)
# ============================================================

# Reload models needed for combined plot
AvThatch_cm.m1.brms <- readRDS("Output/thatch_model.rds")
VisObs_Av.m1.brms   <- readRDS("Output/visobs_model.rds")
bareground.m1.brms  <- readRDS("Output/bareground_model.rds")
AllYield.m1.brms    <- readRDS("Output/yield_model.rds")
thatch.m1.brms      <- readRDS("Output/thatch_biomass_model.rds")
Mustard.m1.brms      <- readRDS("Output/mustard_cover_model.rds")


# Rebuild each panel without titles (cleaner in combined plot)
panel_thatch <- make_ce_plot(AvThatch_cm.m1.brms,
                             title = "Thatch Height (cm)",
                             ylab  = "Thatch Height (cm)")

panel_visobs <- make_ce_plot(VisObs_Av.m1.brms,
                             title = "Visual Obstruction (cm)",
                             ylab  = "Visual Obstruction (cm)")

panel_bg <- make_ce_plot(bareground.m1.brms,
                         title = "Bare Ground (proportion)",
                         ylab  = "Bare Ground (proportion)")

panel_yield <- make_ce_plot(AllYield.m1.brms,
                            title = "Yield (lbs/ac)",
                            ylab  = "Yield (lbs/ac)")

# glmer panels using ggpredict
panel_vegheight <- ggpredict(AvVegHeight.m1,
                             terms = c("Year.f", "Subplot", "Plot"),
                             type  = "random") |>
  plot() +
  theme_pinn +
  ylab("Veg Height (cm)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~facet) +
  ggtitle("Vegetation Height (cm)") 

panel_mustard <- ggpredict(Mustard.m1,
                           terms = c("Year.f", "Subplot", "Plot"),
                           type  = "random") |>
  plot() +
  theme_pinn +
  ylab("Mustard Cover") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~facet) +
  ggtitle("Mustard Cover (proportion)") 

panel_mustarddens <- ggpredict(MustardDens.m1,
                               terms = c("Year.f", "Subplot", "Plot"),
                               type  = "fixed") |>
  plot() +
  theme_pinn +
  ylab("Mustard Density (plants/m²)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~facet) +
  ggtitle("Mustard Density (plants/m²)")

panel_thatch_bio <- ggpredict(thatch.m1.brms,
                              terms = c("Year.f", "Subplot", "Plot"),
                              type  = "fixed") |>
  plot() +
  theme_pinn +
  ylab("Thatch (lbs/ac)") +
  xlab("Year") +
  scale_color_manual(values = subtrt_colors) +
  scale_fill_manual(values  = subtrt_colors) +
  facet_wrap(~facet) +
  ggtitle("Thatch Biomass (lbs/ac)")

# Combine with patchwork
combined_plot <-
  (panel_thatch    | panel_visobs) /
  (panel_bg        | panel_yield)  /
  (panel_vegheight | panel_mustard) /
  (panel_thatch_bio | panel_mustarddens) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title   = "Treatment Effects on Habitat Variables",
    subtitle = "Posterior predicted means with 95% credible intervals",
    theme   = theme(plot.title    = element_text(size = 18, face = "bold"),
                    plot.subtitle = element_text(size = 14))
  ) &
  theme(legend.position = "bottom")

combined_plot
ggsave("Output/Combined_effects.png", combined_plot,
       width = 80, height = 80, units = "cm", dpi = 200)










# Clean up
rm(AvThatch_cm.m1.brms, VisObs_Av.m1.brms,
   bareground.m1.brms, AllYield.m1.brms,
   panel_thatch, panel_visobs, panel_bg, panel_yield,
   panel_vegheight, panel_mustard, panel_mustarddens,
   panel_thatch_bio, combined_plot)
gc()