# ============================================================
# GLLVM - FULL CLEAN SCRIPT
# ============================================================
library(gllvm)
library(parallel)
library(doParallel)
par(mfrow = c(1, 1))

# ============================================================
# CHECK DATA STRUCTURE
# ============================================================
str(Wide_7.FxlGrp)
str(Wide_7.env)
table(Wide_7.env$Treatment, Wide_7.env$SubTrt)
levels(Wide_7.env$Subtreatment)

# ============================================================
# FIT ADDITIVE MODEL
# ============================================================
fit_env.nb <- gllvm(
  Wide_7.FxlGrp,
  Wide_7.env,
  family  = "negative.binomial",
  method  = "VA",
  num.lv  = 1,
  formula = ~ Subtreatment + Year.f + (1|Plot),
  seed    = 1234
)

# Convergence and diagnostics
fit_env.nb$convergence
plot(fit_env.nb, mfrow = c(3, 2))
summary(fit_env.nb)

# Save model
saveRDS(fit_env.nb, "Output/gllvm_model.rds")
# fit_env.nb <- readRDS("Output/gllvm_model.rds")

# Built-in diagnostic plots
par(mfrow = c(1, 1))
gllvm::ordiplot(fit_env.nb, biplot = TRUE)
coefplot(fit_env.nb,
         cex.ylab = 0.7,
         mar      = c(4, 5, 2, 1),
         mfrow    = c(3, 4),
         order    = FALSE)

# ============================================================
# SHARED LABELS AND COLORS
# ============================================================
covariate_labels <- c(
  "SubtreatmentUngrazed + Scraped + Seeded"         = "Ungrazed + S+S",
  "SubtreatmentUngrazed + Mowed"                    = "Ungrazed + Mowed",
  "SubtreatmentUngrazed + Mowed + Scraped + Seeded" = "Ungrazed + M+S+S",
  "SubtreatmentGrazed"                              = "Grazed",
  "SubtreatmentGrazed + Scraped + Seeded"           = "Grazed + S+S",
  "SubtreatmentSeasonal Graze"                      = "Seasonal",
  "SubtreatmentSeasonal Graze + Scraped + Seeded"   = "Seasonal + S+S",
  "Year.f2022"                                      = "2022",
  "Year.f2023"                                      = "2023",
  "Year.f2024"                                      = "2024",
  "Year.f2025"                                      = "2025"
)

all_species_labels <- c(
  "HIIN"            = "Summer Mustard",
  "CESO"            = "Yellow Star Thistle",
  "ESCA"            = "California Poppy",
  "DELO"            = "Meadowfoam",
  "CAEX"            = "California Oatgrass",
  "LAGR"            = "Clarkia",
  "Native Forb"     = "Native Forb",
  "Native Grass"    = "Native Grass",
  "Nonnative Forb"  = "Non-native Forb",
  "Nonnative Grass" = "Non-native Grass",
  "BareGround"      = "Bare Ground",
  "DeadSum"         = "Dead Material"
)

focus_species_labels <- c(
  "HIIN" = "Summer Mustard",
  "CESO" = "Yellow Star Thistle",
  "ESCA" = "California Poppy",
  "DELO" = "Meadowfoam",
  "CAEX" = "California Oatgrass",
  "LAGR" = "Clarkia"
)

type_colors_gllvm <- c(
  "Summer Mustard"      = "#E69A00",
  "Yellow Star Thistle" = "darkred",
  "Wildflower"          = "goldenrod",
  "Native"              = "darkblue",
  "Non-native"          = "#CC4E00",
  "Bare Ground"         = "tan4",
  "Dead"                = "gray40",
  "Other"               = "black"
)

focus_species <- c("HIIN", "CESO", "ESCA", "DELO", "CAEX", "LAGR")

# Helper function for species type assignment
assign_type <- function(species_vec) {
  case_when(
    species_vec == "HIIN"                                   ~ "Summer Mustard",
    species_vec == "CESO"                                   ~ "Yellow Star Thistle",
    species_vec %in% c("ESCA", "DELO", "CAEX", "LAGR")     ~ "Wildflower",
    species_vec %in% c("Native Forb", "Native Grass")       ~ "Native",
    species_vec %in% c("Nonnative Forb", "Nonnative Grass") ~ "Non-native",
    species_vec == "BareGround"                             ~ "Bare Ground",
    species_vec == "DeadSum"                                ~ "Dead",
    .default                                                = "Other"
  )
}

# ============================================================
# TIDY COEFFICIENT DATA FRAME
# ============================================================
glvvm.coef.plot <- bind_cols(
  fit_env.nb[["params"]][["Xcoef"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Species") %>%
    pivot_longer(-Species,
                 names_to  = "Covariate",
                 values_to = "Estimate"),
  fit_env.nb[["sd"]][["Xcoef"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Species") %>%
    pivot_longer(-Species,
                 names_to  = "Covariate",
                 values_to = "Estimate.sd") %>%
    dplyr::select(Estimate.sd)
) %>%
  mutate(
    Type        = assign_type(Species),
    CI_low      = Estimate - 1.96 * Estimate.sd,
    CI_high     = Estimate + 1.96 * Estimate.sd,
    Significant = sign(CI_low) == sign(CI_high)
  )

# Verify covariate names match labels
unique(glvvm.coef.plot$Covariate)

# ============================================================
# COEFFICIENT PLOT 1: BY TYPE
# ============================================================
p.gllvm.type <- glvvm.coef.plot %>%
  filter(Species != "XXX") %>%
  ggplot(aes(reorder(Type, -Estimate), Estimate, color = Type)) +
  geom_pointrange(aes(
    ymin  = CI_low,
    ymax  = CI_high,
    alpha = Significant
  )) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.3),
                     guide  = "none") +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() +
  facet_wrap(. ~ Covariate,
             labeller = labeller(Covariate = covariate_labels)) +
  scale_color_manual(values = type_colors_gllvm) +
  ylim(-20, 20) +
  xlab(NULL) +
  theme_gray(base_size = 14) +
  theme(legend.position = "none") +
  ggtitle("Species Group Responses to Treatment and Year")

p.gllvm.type
ggsave("Output/gllvm_coef_type.png", p.gllvm.type,
       width = 35, height = 20, units = "cm", dpi = 300)
rm(p.gllvm.type)
gc()

# ============================================================
# COEFFICIENT PLOT 2: BY SPECIES
# ============================================================
p.gllvm.species.coef <- glvvm.coef.plot %>%
  filter(Species != "XXX") %>%
  ggplot(aes(reorder(Species, -Estimate), Estimate, color = Type)) +
  geom_pointrange(aes(
    ymin  = CI_low,
    ymax  = CI_high,
    alpha = Significant
  )) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.3),
                     guide  = "none") +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() +
  facet_wrap(. ~ Covariate,
             labeller = labeller(Covariate = covariate_labels)) +
  scale_color_manual(values = type_colors_gllvm) +
  ylim(-15, 17) +
  xlab(NULL) +
  theme_gray(base_size = 10) +
  theme(
    legend.position = c(0.85, 0.08),
    legend.key.size = unit(0.5, "cm")
  ) +
  ggtitle("Species Responses to Treatment and Year")

p.gllvm.species.coef
ggsave("Output/gllvm_coef_species.png", p.gllvm.species.coef,
       width = 35, height = 30, units = "cm", dpi = 300)
rm(p.gllvm.species.coef)
gc()

# ============================================================
# BUILD PREDICTION GRID
# ============================================================
pred_grid <- expand.grid(
  Subtreatment = levels(Wide_7.env$Subtreatment),
  Year.f        = levels(Wide_7.env$Year.f)
) %>%
  mutate(
    Plot      = factor(1, levels = levels(Wide_7.env$Plot)),
    Treatment = factor(case_when(
      grepl("Grazed$",    Subtreatment) |
        grepl("Grazed \\+", Subtreatment) ~ "Grazed",
      grepl("Seasonal",   Subtreatment)   ~ "Seasonal Graze",
      .default                            = "Ungrazed"
    ), levels = levels(Wide_7.env$Treatment)),
    SubTrt    = factor(case_when(
      grepl("Mowed", Subtreatment) &
        grepl("Scraped", Subtreatment)    ~ "Mowed + Scraped + Seeded",
      grepl("Mowed",    Subtreatment)     ~ "Mowed",
      grepl("Scraped",  Subtreatment)     ~ "Scraped + Seeded",
      .default                           = "Control"
    ), levels = levels(Wide_7.env$SubTrt))
  )

cat("Prediction grid:", nrow(pred_grid), "rows\n")

# Point predictions
preds <- predict(fit_env.nb,
                 newX    = pred_grid,
                 newdata = pred_grid,
                 type    = "response",
                 level   = 0)

dim(preds)  # should be 40 x 12

# ============================================================
# BOOTSTRAP SEs — all 64 cores
# ============================================================
n_boot  <- 100
n_cores <- parallel::detectCores()
cat("Using", n_cores, "cores for bootstrap\n")

cl <- makeCluster(n_cores)
registerDoParallel(cl)
clusterExport(cl, c("Wide_7.FxlGrp", "Wide_7.env", "pred_grid"))
clusterEvalQ(cl, library(gllvm))

set.seed(123)
boot_preds <- parLapply(cl, 1:n_boot, function(i) {
  tryCatch({
    idx      <- sample(nrow(Wide_7.env), replace = TRUE)
    boot_fit <- gllvm(Wide_7.FxlGrp[idx, ], Wide_7.env[idx, ],
                      family  = "negative.binomial",
                      method  = "VA",
                      num.lv  = 1,
                      formula = ~ Subtreatment + Year.f + (1|Plot),
                      seed    = i)
    predict(boot_fit,
            newX    = pred_grid,
            newdata = pred_grid,
            type    = "response",
            level   = 0)
  }, error = function(e) NULL)
})

stopCluster(cl)

boot_preds <- boot_preds[!sapply(boot_preds, is.null)]
cat("Successful bootstraps:", length(boot_preds), "of", n_boot, "\n")

# ============================================================
# BUILD pred_df WITH CIs
# ============================================================
boot_array  <- simplify2array(boot_preds)  # 40 x 12 x n_boot
pred_se_mat <- apply(boot_array, c(1, 2), sd)

cat("SE range:", range(pred_se_mat), "\n")

pred_df <- as.data.frame(preds) %>%
  bind_cols(pred_grid) %>%
  pivot_longer(
    cols      = all_of(colnames(Wide_7.FxlGrp)),
    names_to  = "Species",
    values_to = "Predicted"
  )

pred_se_df <- as.data.frame(pred_se_mat) %>%
  setNames(colnames(Wide_7.FxlGrp)) %>%
  bind_cols(pred_grid) %>%
  pivot_longer(
    cols      = all_of(colnames(Wide_7.FxlGrp)),
    names_to  = "Species",
    values_to = "SE"
  ) %>%
  dplyr::select(SE)

pred_df <- bind_cols(pred_df, pred_se_df) %>%
  mutate(
    CI_low  = pmax(0, Predicted - 1.96 * SE),
    CI_high = Predicted + 1.96 * SE,
    Type    = assign_type(Species)
  )

cat("pred_df dimensions:", nrow(pred_df), "rows\n")
summary(pred_df$SE)

saveRDS(pred_df, "Output/gllvm_pred_df_with_CI.rds")
# pred_df <- readRDS("Output/gllvm_pred_df_with_CI.rds")

# ============================================================
# PREDICTION PLOT 1: ALL SPECIES WITH RIBBONS
# ============================================================
p.gllvm.pred.species <- pred_df %>%
  ggplot(aes(x     = Year.f,
             y     = Predicted,
             color = Subtreatment,
             group = Subtreatment)) +
  geom_ribbon(aes(ymin = CI_low,
                  ymax = CI_high,
                  fill = Subtreatment),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_color_manual(values = subtreatment_colors) +
  scale_fill_manual(values  = subtreatment_colors, guide = "none") +
  facet_wrap(~Species,
             scales   = "free_y",
             labeller = labeller(Species = all_species_labels)) +
  theme_gray(base_size = 12) +
  xlab("Year") +
  ylab("Predicted abundance") +
  labs(color = NULL) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  ggtitle("GLLVM Predicted Abundance by Species, Subtreatment and Year")

p.gllvm.pred.species
ggsave("Output/gllvm_predicted_species.png", p.gllvm.pred.species,
       width = 40, height = 30, units = "cm", dpi = 300)
rm(p.gllvm.pred.species)
gc()

# ============================================================
# PREDICTION PLOT 2: BY TYPE WITH RIBBONS
# ============================================================
pred_df_type <- pred_df %>%
  group_by(Year.f, Subtreatment, Type) %>%
  summarise(
    Mean_pred    = mean(Predicted, na.rm = TRUE),
    Mean_CI_low  = mean(CI_low,    na.rm = TRUE),
    Mean_CI_high = mean(CI_high,   na.rm = TRUE),
    .groups      = "drop"
  )

p.gllvm.pred.type <- pred_df_type %>%
  ggplot(aes(x     = Year.f,
             y     = Mean_pred,
             color = Subtreatment,
             group = Subtreatment)) +
  geom_ribbon(aes(ymin = Mean_CI_low,
                  ymax = Mean_CI_high,
                  fill = Subtreatment),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = subtreatment_colors) +
  scale_fill_manual(values  = subtreatment_colors, guide = "none") +
  facet_wrap(~Type, scales = "free_y") +
  theme_gray(base_size = 14) +
  xlab("Year") +
  ylab("Mean predicted abundance") +
  labs(color = NULL) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  ggtitle("GLLVM Predicted Abundance by Species Type, Subtreatment and Year")

p.gllvm.pred.type
ggsave("Output/gllvm_predicted_type.png", p.gllvm.pred.type,
       width = 35, height = 25, units = "cm", dpi = 300)
rm(p.gllvm.pred.type, pred_df_type)
gc()

# ============================================================
# PREDICTION PLOT 3: FOCUS SPECIES WITH RIBBONS
# ============================================================
p.gllvm.pred.focus <- pred_df %>%
  filter(Species %in% focus_species) %>%
  ggplot(aes(x     = Year.f,
             y     = Predicted,
             color = Subtreatment,
             group = Subtreatment)) +
  geom_ribbon(aes(ymin = CI_low,
                  ymax = CI_high,
                  fill = Subtreatment),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 1.5,
             linetype   = 4,
             color      = "gray40") +
  scale_color_manual(values = subtreatment_colors) +
  scale_fill_manual(values  = subtreatment_colors, guide = "none") +
  facet_wrap(~Species,
             scales   = "free_y",
             ncol     = 3,
             labeller = labeller(Species = focus_species_labels)) +
  theme_gray(base_size = 14) +
  xlab("Year") +
  ylab("Predicted abundance") +
  labs(color = NULL) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  ggtitle("GLLVM Predicted Abundance — Target Species by Subtreatment and Year")

p.gllvm.pred.focus
ggsave("Output/gllvm_predicted_focus.png", p.gllvm.pred.focus,
       width = 35, height = 25, units = "cm", dpi = 300)
rm(p.gllvm.pred.focus)
gc()