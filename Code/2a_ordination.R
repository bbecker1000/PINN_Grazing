par(mfrow = c(1,1))

#source("Code/1_DataPrep.R")

## nms
# NMS
ord <- metaMDS(Wide_7.FxlGrp, trymax = 100)  #was wide_7 with all groups. no solutions
ord
plot(ord)

stressplot(ord)

adonis2.mod <- 
  adonis2(Wide_7.FxlGrp ~ PPT_CM + Year.f * Treatment * SubTrt,
          data = Wide_7.env,
          permutations = 1000,
          parallel = 16, 
          strata = Wide_7.env$Plot,
          by = "terms")
adonis2.mod


ord.fit <- envfit(ord ~ PPT_CM + Year.f * Treatment * SubTrt, data=Wide_7.env, perm=1000) ## envfit doesn't seem to do interactions
(ord.fit)
summary(ord.fit)

ord.fit$vectors$arrows

plot(ord, dis="site")
plot(ord.fit)




# #ggord plots
# ggord(ord,
#       grp_in = paste(Wide_7.FxlGrp$PrePost.f, Wide_7.env$Rain.f), # Wide_6.env$Subtreatment, 
#       #grp_in = paste(Wide_6.env$Subtreatment, Wide_6.env$Year),
#       arrow = NULL,
#       size = 1,
#       alpha_el = 0.3,
#       facet = FALSE, 
#       repel = FALSE,
#       txt = 2,
#       nfac=4
# )
# 
# ggord(ord,
#       grp_in = paste(Wide_7.env$Year.f, Wide_7.env$Subtreatment), 
#       #grp_in = paste(Wide_6.env$Subtreatment, Wide_6.env$Year),
#       arrow = NULL,
#       size = 1,
#       alpha = 0.1,
#       alpha_el = 0.3,
#       facet = TRUE, 
#       repel = FALSE,
#       txt = 2,
#       nfac= 6
# ) +
#   theme(legend.position = 'none')


# make the species locations NMS plot
Species.plot.df <- rownames_to_column(as.data.frame(ord[["species"]]), var = "Species") %>% as_tibble()
Species.plot.df$Target <- ifelse(Species.plot.df$Species == "HIIN", "Weed",
                                 ifelse(Species.plot.df$Species == "CESO", "Weed",
                                        ifelse(Species.plot.df$Species == "ESCA", "Wildflower",
                                               ifelse(Species.plot.df$Species == "DELO", "Wildflower",
                                                      ifelse(Species.plot.df$Species == "CAEX", "Wildflower",
                                                             ifelse(Species.plot.df$Species == "LAGR", "Wildflower",
                                                                    "Other"))))))


p <- ggplot(Species.plot.df, aes(x= MDS1, y= MDS2, label = Species, color = Target)) +
  geom_text_repel(max.overlaps = 15) +
  xlim(c(-1,1.5)) +
  ylim(c(-1.5,1.25))
p

ggsave("Output/NMS.jpg", width = 25, height = 25, units = "cm")




#make custom nms data frame
points <- data.frame(ord[["points"]])

#join points to 
df.ord <- cbind(Wide_7.env, points)


species <- data.frame(ord[["species"]])
species <- tibble::rownames_to_column(species, "species")
#HIIN and CESO are weeds

species$type <- c("Non-native", "Native", "Wildflower", "Weed", 
                  "Native", "Wildflower", "Non-native", "Wildflower", "Other",
                  "Weed", "Wildflower", "Other", "Other")

#plot years, treatments, and species
library(RColorBrewer)
library(paletteer)
library(ggrepel)

p.nms.Trt <- 
  ggplot() +
  geom_point(data = df.ord, aes(x = MDS1, y = MDS2, color = SubTrt)) +
  stat_ellipse(geom = "polygon", data = df.ord, aes(x = MDS1, y = MDS2, 
                                                    fill = Treatment, color = Treatment), 
               alpha = 0.2, level = 0.8) +
  geom_text_repel(data = species, aes(x=MDS1, y=MDS2, label = species, color = type)) +
  facet_wrap(.~Year.f, ncol = 3)
p.nms.Trt + scale_color_paletteer_d("ggsci::default_uchicago") +
  scale_fill_paletteer_d("ggsci::default_uchicago")

ggsave("Output/nms_SubTrt.png", width = 25, height = 20, units = "cm")







# add envfit arrows
# Define distinct colors for SubTrt (points/ellipses) and type (text labels)
subtrt_colors <- c(
  "Control"                = "#8B0000",   # dark red
  "Scraped + Seeded"       = "#4A4A4A",   # dark gray
  "Mowed"                  = "#E69A00",   # amber
  "Mowed + Scraped + Seeded" = "#4D7A3A"  # dark green
)

type_colors <- c(
  "Native"     = "darkblue",   # strong blue
  "Non-native" = "#CC4E00",   # burnt orange
  "Other"      = "black",   
  "Weed"       = "#006B6B",   # teal
  "Wildflower" = "#B8006A"    # deep pink
)

# Add rainfall arrow as a geom_segment so it appears in the legend
rain_arrow <- data.frame(
  x    = 0,
  y    = 0,
  xend = ord.fit$vectors$arrows["PPT_CM", "NMDS1"],
  yend = ord.fit$vectors$arrows["PPT_CM", "NMDS2"]
)

#facet labels
year_labels <- c(
  "2021" = "2021 (Pre-Treatment)",
  "2022" = "2022",
  "2023" = "2023",
  "2024" = "2024",
  "2025" = "2025"
)

p.nms.SubTrt <- 
  ggplot() +
  geom_point(data = df.ord, aes(x = MDS1, y = MDS2, color = SubTrt)) +
  stat_ellipse(geom = "polygon", data = df.ord, 
               aes(x = MDS1, y = MDS2, fill = SubTrt, color = SubTrt), 
               alpha = 0.1, level = 0.89) +
  geom_text_repel(data = species, 
                  aes(x = MDS1, y = MDS2, label = species, color = type)) +
  # Dummy segment for legend (no arrow)
  geom_segment(data = rain_arrow,
               aes(x = x, y = y, xend = xend, yend = yend, linetype = "Rainfall"),
               color = "darkblue",
               arrow = arrow(length = unit(0.5, "cm"))) +
  theme_gray(base_size = 14) +
  facet_wrap(.~Year.f, labeller = labeller(Year.f = year_labels)) +
  scale_color_manual(
    values = c(subtrt_colors, type_colors),
    breaks = c(names(subtrt_colors), names(type_colors)),
    guide = guide_legend(override.aes = list(
      linetype = c(rep("solid", 4), rep("blank", 5)),
      shape    = c(rep(16, 4), rep(65, 5))
    ))
  ) +
  scale_fill_manual(
    values = subtrt_colors,
    guide = "none"
  ) +
  # Just show a plain blue line in the legend — arrow renders on the plot itself
  scale_linetype_manual(
    values = c("Rainfall" = "solid"),
    guide = guide_legend(
      override.aes = list(color = "darkblue", size = 0.75)
    )
  ) +
  labs(color = NULL, linetype = NULL)

p.nms.SubTrt
ggsave("Output/nms_SubTrt.png", width = 35, height = 20, units = "cm")



####






rain <- as_tibble(ord.fit[["vectors"]][["arrows"]])
factors <- as_tibble(ord.fit[["factors"]][["centroids"]])


# Define rainfall colors (adjust levels to match your actual Rain.f levels)
rain_colors <- c(
  "Low"    = "#6BAED6",   # light blue
  "Med" = "#2171B5",   # medium blue
  "High"   = "#08306B"    # dark blue
)

p.nms.Rain <- 
  ggplot() +
  geom_point(data = df.ord, aes(x = MDS1, y = MDS2, color = Rain.f)) +
  stat_ellipse(geom = "polygon", data = df.ord, 
               aes(x = MDS1, y = MDS2, fill = Rain.f, color = Rain.f), 
               alpha = 0.15, level = 0.89) +
  geom_text_repel(data = species, 
                  aes(x = MDS1, y = MDS2, label = species, color = type)) +
  geom_segment(data = rain_arrow,
               aes(x = x, y = y, xend = xend, yend = yend, linetype = "Rainfall"),
               color = "darkblue",
               arrow = arrow(length = unit(0.5, "cm"))) +
  theme_gray(base_size = 14) +
  #facet_wrap(.~Year.f, labeller = labeller(Year.f = year_labels)) +
  scale_color_manual(
    values = c(rain_colors, type_colors),
    breaks = c(names(rain_colors), names(type_colors)),
    guide = guide_legend(override.aes = list(
      linetype = c(rep("solid", length(rain_colors)), rep("blank", length(type_colors))),
      shape    = c(rep(16, length(rain_colors)), rep(65, length(type_colors)))
    ))
  ) +
  scale_fill_manual(
    values = rain_colors,
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c("Rainfall" = "solid"),
    guide = guide_legend(
      override.aes = list(color = "darkblue", size = 0.75)
    )
  ) +
  labs(color = NULL, linetype = NULL)

p.nms.Rain
ggsave("Output/nms_Rain.png", width = 35, height = 20, units = "cm")





