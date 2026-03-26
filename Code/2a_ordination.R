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
# ENVFIT
# ============================================================
ord.fit <- envfit(
  ord ~ PPT_CM + Year.f * Treatment * SubTrt,
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
    species == "HIIN"         ~ "Weed",
    species == "CESO"         ~ "Weed",
    species %in% c("ESCA", "DELO", "CAEX", "LAGR") ~ "Wildflower",
    species %in% c("Native-AF", "Native-PG")        ~ "Native",
    species %in% c("NonNative-AF", "NonNative-AG")  ~ "Non-native",
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
subtrt_colors <- c(
  "Control"                  = "#8B0000",
  "Scraped + Seeded"         = "#4A4A4A",
  "Mowed"                    = "#E69A00",
  "Mowed + Scraped + Seeded" = "#4D7A3A"
)

type_colors <- c(
  "Native"     = "darkblue",
  "Non-native" = "#CC4E00",
  "Other"      = "black",
  "Weed"       = "#006B6B",
  "Wildflower" = "#B8006A"
)

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

# Reusable legend guide for linetype
rainfall_linetype_guide <- scale_linetype_manual(
  values = c("Rainfall" = "solid"),
  guide  = guide_legend(
    override.aes = list(color = "darkblue", size = 0.75)
  )
)

# Reusable rainfall arrow geom
rainfall_arrow_geom <- geom_segment(
  data  = rain_arrow,
  aes(x = x, y = y, xend = xend, yend = yend, linetype = "Rainfall"),
  color = "darkblue",
  arrow = arrow(length = unit(0.5, "cm"))
)

# ============================================================
# PLOT 1: NMS BY SUBTREATMENT
# ============================================================
p.nms.SubTrt <-
  ggplot() +
  geom_point(data = df.ord,
             aes(x = MDS1, y = MDS2, color = SubTrt)) +
  stat_ellipse(geom  = "polygon", data = df.ord,
               aes(x = MDS1, y = MDS2, fill = SubTrt, color = SubTrt),
               alpha = 0.1, level = 0.89) +
  geom_text_repel(data = species,
                  aes(x = MDS1, y = MDS2, label = species, color = type)) +
  rainfall_arrow_geom +
  theme_gray(base_size = 14) +
  facet_wrap(. ~ Year.f, labeller = labeller(Year.f = year_labels)) +
  scale_color_manual(
    values = c(subtrt_colors, type_colors),
    breaks = c(names(subtrt_colors), names(type_colors)),
    guide  = guide_legend(override.aes = list(
      linetype = c(rep("solid", length(subtrt_colors)),
                   rep("blank",  length(type_colors))),
      shape    = c(rep(16, length(subtrt_colors)),
                   rep(65, length(type_colors)))
    ))
  ) +
  scale_fill_manual(values = subtrt_colors, guide = "none") +
  rainfall_linetype_guide +
  labs(color = NULL, linetype = NULL)

p.nms.SubTrt
ggsave("Output/nms_SubTrt.png", p.nms.SubTrt,
       width = 35, height = 20, units = "cm")
rm(p.nms.SubTrt)
gc()

# ============================================================
# PLOT 2: NMS BY RAINFALL
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



