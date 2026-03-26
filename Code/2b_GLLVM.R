# ============================================================
# GLLVM
# ============================================================
library(gllvm)
par(mfrow = c(1, 1))

# ============================================================
# FIT MODEL
# ============================================================
fit_env.nb <- gllvm(
  Wide_7.FxlGrp,
  Wide_7.env,
  family  = "negative.binomial",
  method  = "VA",   #runs about 2-3 min with VA.   "LA" takes 5+ minutes
  num.lv  = 1,
  formula = ~ Treatment + Year.f + Rain.f + (1|Plot),
  seed    = 1234
)

# Convergence and diagnostics
fit_env.nb$convergence       # should be TRUE
plot(fit_env.nb, mfrow = c(3, 2))
summary(fit_env.nb)

# Ordination and coefficient plots
par(mfrow = c(1, 1))
gllvm::ordiplot(fit_env.nb, biplot = TRUE)
coefplot(fit_env.nb,
         cex.ylab = 0.7,
         mar      = c(4, 5, 2, 1),
         mfrow    = c(3, 4),
         order    = FALSE)

# ============================================================
# TIDY COEFFICIENT DATA FRAME
# ============================================================
glvvm.coef.plot <- bind_cols(
  fit_env.nb[["params"]][["Xcoef"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Species") %>%
    pivot_longer(-Species, names_to = "Covariate", values_to = "Estimate"),
  fit_env.nb[["sd"]][["Xcoef"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Species") %>%
    pivot_longer(-Species, names_to = "Covariate", values_to = "Estimate.sd") %>%
    dplyr::select(Estimate.sd)
) %>%
  # Assign species types
  mutate(Type = case_when(
    Species == "HIIN"                                                 ~ "Summer Mustard",
    Species == "CESO"                                                 ~ "Yellow Star Thistle",
    Species %in% c("ESCA", "DELO", "CAEX", "LAGR")                   ~ "Wildflower",
    Species %in% c("ACAM", "AMSIN", "CACO", "CAME", "CLPU", "CRSE",
                   "EUCH", "LUBI", "TRGR", "TRIFO")                  ~ "Native Forb",
    Species %in% c("CEGL", "ERBO", "ERCI", "ERMO", "LAAM",
                   "MEPO", "STME", "VEPE")                            ~ "Non-Native Forb",
    Species == "BareGround"                                           ~ "Bare Ground",
    Species == "DeadSum"                                              ~ "Dead",
    .default = "Other"
  ))

# ============================================================
# COVARIATE LABELS
# ============================================================
covariate_labels <- c(
  "Rain.fHigh"              = "Rain High",
  "Rain.fMed"               = "Rain Med",
  "TreatmentGrazed"         = "Grazed",
  "TreatmentSeasonal.Graze" = "Seasonal Grazed",
  "X1...PlotTRUE"           = "Post Treatment",
  "Year.f2022"              = "2022",
  "Year.f2023"              = "2023",
  "Year.f2024"              = "2024",
  "Year.f2025"              = "2025"
)

# ============================================================
# PLOT 1: COEFFICIENT PLOT BY TYPE
# ============================================================
p.gllvm.type <- glvvm.coef.plot %>%
  filter(Species != "XXX") %>%
  ggplot(aes(reorder(Type, -Estimate), Estimate, color = Type)) +
  geom_pointrange(aes(
    ymin = Estimate - 2 * Estimate.sd,
    ymax = Estimate + 2 * Estimate.sd
  )) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() +
  facet_wrap(. ~ Covariate,
             labeller = labeller(Covariate = covariate_labels)) +
  ylim(-20, 20) +
  xlab(NULL) +
  theme_gray(base_size = 16) +
  theme(legend.position = "none")

p.gllvm.type
ggsave("Output/glvvmPlot.jpg", p.gllvm.type,
       width = 30, height = 20, units = "cm")
rm(p.gllvm.type)
gc()

# ============================================================
# PLOT 2: COEFFICIENT PLOT BY SPECIES
# ============================================================
p.gllvm.species <- glvvm.coef.plot %>%
  filter(Species != "XXX") %>%
  ggplot(aes(reorder(Species, -Estimate), Estimate, color = Type)) +
  geom_pointrange(aes(
    ymin = Estimate - 2 * Estimate.sd,
    ymax = Estimate + 2 * Estimate.sd
  )) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() +
  facet_wrap(. ~ Covariate,
             labeller = labeller(Covariate = covariate_labels)) +
  ylim(-15, 17) +
  xlab(NULL) +
  theme_gray(base_size = 10) +
  theme(
    legend.position  = c(0.8, 0.12),
    legend.key.size  = unit(0.5, "cm")
  )

p.gllvm.species
ggsave("Output/glvvmPlotBySpecies.jpg", p.gllvm.species,
       width = 30, height = 30, units = "cm")
rm(p.gllvm.species)
gc()


