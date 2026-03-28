par(mfrow = c(1,1))

# ============================================================
# NMS ORDINATION
# ============================================================
ord <- metaMDS(Wide_7.FxlGrp, trymax = 100)
ord
plot(ord)
stressplot(ord)

# ============================================================
# PERMANOVA
# ============================================================
adonis2.mod <- adonis2(
  Wide_7.FxlGrp ~ PPT_CM + Year.f * Treatment * SubTrt,
  data         = Wide_7.env,
  permutations = 1000,
  parallel     = 16,
  strata       = Wide_7.env$Plot,
  by           = "terms"
)
adonis2.mod


# ============================================================
# PERMANOVA complex
# ============================================================
adonis3.mod <- adonis2(
  Wide_7.FxlGrp ~ PPT_CM * PrePost * Treatment * SubTrt,
  data         = Wide_7.env,
  permutations = 1000,
  parallel     = 16,
  strata       = Wide_7.env$Plot,
  by           = "terms"
)
adonis3.mod


##permanova rainfall interaction
adonis2.mod.b <- adonis2(
  Wide_7.FxlGrp ~ PPT_CM + Year.f + Treatment + SubTrt + 
    PPT_CM:SubTrt + Year.f:SubTrt + Year.f:Treatment,
  data         = Wide_7.env,
  permutations = 1000,
  parallel     = 64,
  strata       = Wide_7.env$Plot,
  by           = "terms"
)
adonis2.mod.b


# Total sum of squares is the last row of SumOfSqs
adonis2.mod["Total", "SumOfSqs"]    # original
adonis2.mod.b["Total", "SumOfSqs"]  # model b


## let's use subtreatment since not fully crossed:

adonis2.mod <- adonis2(
  Wide_7.FxlGrp ~ PPT_CM + Year.f * Subtreatment,
  data         = Wide_7.env,
  permutations = 999,
  parallel     = 64,
  strata       = Wide_7.env$Plot,
  by           = "terms"
)
adonis2.mod

# compare models


# ============================================================
# ENVFIT
# ============================================================
ord.fit <- envfit(
  ord ~ PPT_CM + Year.f * Subtreatment,
  data = Wide_7.env,
  perm = 1000
)
ord.fit
summary(ord.fit)

plot(ord, dis = "site")
plot(ord.fit)

# ============================================================
# BUILD ORDINATION DATA FRAMES
# ============================================================

# Site scores
df.ord <- cbind(Wide_7.env, data.frame(ord[["points"]]))

# Species scores with type assignments
species <- ord[["species"]] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("species") %>%
  mutate(type = case_when(
    species == "HIIN"                                        ~ "Invasive Weed",
    species == "CESO"                                        ~ "Invasive Weed",
    species %in% c("ESCA", "DELO", "CAEX", "LAGR")          ~ "Wildflower",
    species %in% c("Native Forb", "Native Grass")            ~ "Native",
    species %in% c("Nonnative Forb", "Nonnative Grass")      ~ "Non-native",
    .default = "Other"
  ))

# Rainfall arrow from envfit
rain_arrow <- data.frame(
  x    = 0,
  y    = 0,
  xend = ord.fit$vectors$arrows["PPT_CM", "NMDS1"],
  yend = ord.fit$vectors$arrows["PPT_CM", "NMDS2"]
)

# ============================================================
# COLOR PALETTES
# ============================================================

# 8 Subtreatment colors — blues for Ungrazed, reds for Grazed, greens for Seasonal
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

# 3 Treatment colors
treatment_colors <- c(
  "Ungrazed"       = "#08519C",
  "Grazed"         = "#A50F15",
  "Seasonal Graze" = "#238B45"
)

# Species type colors
type_colors <- c(
  "Native"       = "darkblue",
  "Non-native"   = "#CC4E00",
  "Other"        = "black",
  "Invasive Weed" = "darkred",
  "Wildflower"   = "goldenrod"
)

# Rainfall colors
rain_colors <- c(
  "Low"  = "#6BAED6",
  "Med"  = "#2171B5",
  "High" = "#08306B"
)

# Facet labels
year_labels <- c(
  "2021" = "2021 (Pre-Treatment)",
  "2022" = "2022",
  "2023" = "2023",
  "2024" = "2024",
  "2025" = "2025"
)

# ============================================================
# SHARED PLOT ELEMENTS
# ============================================================
rainfall_linetype_guide <- scale_linetype_manual(
  values = c("Rainfall" = "solid"),
  guide  = guide_legend(
    override.aes = list(color = "darkblue", size = 0.75)
  )
)

rainfall_arrow_geom <- geom_segment(
  data  = rain_arrow,
  aes(x = x, y = y, xend = xend, yend = yend, linetype = "Rainfall"),
  color = "darkblue",
  arrow = arrow(length = unit(0.5, "cm"))
)

# ============================================================
# PLOT 1: NMS BY SUBTREATMENT (8 groups)
# ============================================================
p.nms.Subtreatment <-
  ggplot() +
  geom_point(data = df.ord,
             aes(x = MDS1, y = MDS2, color = Subtreatment)) +
  stat_ellipse(geom  = "polygon", data = df.ord,
               aes(x = MDS1, y = MDS2,
                   fill = Subtreatment, color = Subtreatment),
               alpha = 0.1, level = 0.89) +
  geom_text_repel(data = species,
                  aes(x = MDS1, y = MDS2, label = species, color = type)) +
  rainfall_arrow_geom +
  theme_gray(base_size = 14) +
  facet_wrap(. ~ Year.f, labeller = labeller(Year.f = year_labels)) +
  scale_color_manual(
    values = c(subtreatment_colors, type_colors),
    breaks = c(names(subtreatment_colors), names(type_colors)),
    guide  = guide_legend(override.aes = list(
      linetype = c(rep("solid", length(subtreatment_colors)),
                   rep("blank",  length(type_colors))),
      shape    = c(rep(16, length(subtreatment_colors)),
                   rep(65, length(type_colors)))
    ))
  ) +
  scale_fill_manual(values = subtreatment_colors, guide = "none") +
  rainfall_linetype_guide +
  labs(color = NULL, linetype = NULL)

p.nms.Subtreatment
ggsave("Output/nms_Subtreatment.png", p.nms.SubTrt,
       width = 35, height = 20, units = "cm")
rm(p.nms.SubTrt)
gc()

# ============================================================
# PLOT 2: NMS BY TREATMENT (3 groups — cleaner overview)
# ============================================================
p.nms.Trt <-
  ggplot() +
  geom_point(data = df.ord,
             aes(x = MDS1, y = MDS2, color = Treatment)) +
  stat_ellipse(geom  = "polygon", data = df.ord,
               aes(x = MDS1, y = MDS2,
                   fill = Treatment, color = Treatment),
               alpha = 0.1, level = 0.89) +
  geom_text_repel(data = species,
                  aes(x = MDS1, y = MDS2, label = species, color = type)) +
  rainfall_arrow_geom +
  theme_gray(base_size = 14) +
  facet_wrap(. ~ Year.f, labeller = labeller(Year.f = year_labels)) +
  scale_color_manual(
    values = c(treatment_colors, type_colors),
    breaks = c(names(treatment_colors), names(type_colors)),
    guide  = guide_legend(override.aes = list(
      linetype = c(rep("solid", length(treatment_colors)),
                   rep("blank",  length(type_colors))),
      shape    = c(rep(16, length(treatment_colors)),
                   rep(65, length(type_colors)))
    ))
  ) +
  scale_fill_manual(values = treatment_colors, guide = "none") +
  rainfall_linetype_guide +
  labs(color = NULL, linetype = NULL)

p.nms.Trt
ggsave("Output/nms_Trt.png", p.nms.Trt,
       width = 35, height = 20, units = "cm")
rm(p.nms.Trt)
gc()

# ============================================================
# PLOT 3: NMS BY RAINFALL
# ============================================================
p.nms.Rain <-
  ggplot() +
  geom_point(data = df.ord,
             aes(x = MDS1, y = MDS2, color = Rain.f)) +
  stat_ellipse(geom  = "polygon", data = df.ord,
               aes(x = MDS1, y = MDS2, fill = Rain.f, color = Rain.f),
               alpha = 0.15, level = 0.89) +
  geom_text_repel(data = species,
                  aes(x = MDS1, y = MDS2, label = species, color = type)) +
  rainfall_arrow_geom +
  theme_gray(base_size = 14) +
  scale_color_manual(
    values = c(rain_colors, type_colors),
    breaks = c(names(rain_colors), names(type_colors)),
    guide  = guide_legend(override.aes = list(
      linetype = c(rep("solid", length(rain_colors)),
                   rep("blank",  length(type_colors))),
      shape    = c(rep(16, length(rain_colors)),
                   rep(65, length(type_colors)))
    ))
  ) +
  scale_fill_manual(values = rain_colors, guide = "none") +
  rainfall_linetype_guide +
  labs(color = NULL, linetype = NULL)

p.nms.Rain
ggsave("Output/nms_Rain.png", p.nms.Rain,
       width = 35, height = 20, units = "cm")
rm(p.nms.Rain)
gc()


