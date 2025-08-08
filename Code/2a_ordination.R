par(mfrow = c(1,1))

source("Code/1_DataPrep.R")

## nms
# NMS
ord <- metaMDS(Wide_7.FxlGrp, trymax = 100)  #was wide_7 with all groups. no solutions
ord
plot(ord)

stressplot(ord)

adonis2.mod <- 
  adonis2(Wide_7.FxlGrp ~ Rain.f + Year.f * Treatment * SubTrt,
          data = Wide_7.env,
          permutations = 1000,
          parallel = 16, 
          strata = Wide_7.env$Plot,
          by = "terms")
adonis2.mod



#ggord plots
ggord(ord,
      grp_in = paste(Wide_7.FxlGrp$PrePost.f, Wide_7.env$Rain.f), # Wide_6.env$Subtreatment, 
      #grp_in = paste(Wide_6.env$Subtreatment, Wide_6.env$Year),
      arrow = NULL,
      size = 1,
      alpha_el = 0.3,
      facet = FALSE, 
      repel = FALSE,
      txt = 2,
      nfac=4
)

ggord(ord,
      grp_in = paste(Wide_7.env$Year.f, Wide_7.env$Subtreatment), 
      #grp_in = paste(Wide_6.env$Subtreatment, Wide_6.env$Year),
      arrow = NULL,
      size = 1,
      alpha = 0.1,
      alpha_el = 0.3,
      facet = TRUE, 
      repel = FALSE,
      txt = 2,
      nfac= 6
) +
  theme(legend.position = 'none')


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
               alpha = 0.2) +
  geom_text_repel(data = species, aes(x=MDS1, y=MDS2, label = species, color = type)) +
  facet_wrap(.~Year.f, ncol = 3)
p.nms.Trt + scale_color_paletteer_d("ggsci::default_uchicago") +
  scale_fill_paletteer_d("ggsci::default_uchicago")

ggsave("Output/nms_SubTrt.png", width = 25, height = 20, units = "cm")








p.nms.SubTrt <- 
  ggplot() +
  geom_point(data = df.ord, aes(x = MDS1, y = MDS2, color = SubTrt)) +
  stat_ellipse(geom = "polygon", data = df.ord, aes(x = MDS1, y = MDS2, 
                                                    fill = SubTrt, color = SubTrt), 
                                                    alpha = 0.2) +
  geom_text_repel(data = species, aes(x=MDS1, y=MDS2, label = species, color = type)) +
  facet_wrap(.~Year.f)
p.nms.SubTrt + scale_color_paletteer_d("ggsci::default_uchicago") +
  scale_fill_paletteer_d("ggsci::default_uchicago")

ggsave("Output/nms_SubTrt.png", width = 25, height = 20, units = "cm")



p.nms.Rain <- 
ggplot() +
  geom_point(data = df.ord, aes(x = MDS1, y = MDS2, color = Rain.f)) +
  stat_ellipse(geom = "polygon", data = df.ord, aes(x = MDS1, y = MDS2, 
                                                    fill = Rain.f, color = Rain.f), 
                                                    alpha = 0.2) +
  geom_text_repel(data = species, aes(x=MDS1, y=MDS2, label = species, color = type))
p.nms.Rain + scale_color_paletteer_d("ggsci::default_uchicago") +
  scale_fill_paletteer_d("ggsci::default_uchicago")

ggsave("Output/nms_Rain.png", width = 25, height = 20, units = "cm")

