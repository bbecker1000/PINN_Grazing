##### GLLVM

source("Code/1_DataPrep.R")
library(gllvm)

#reset graphics
par(mfrow = c(1, 1))

#GLLVM neg binomial with covariates
fit_env.nb <- gllvm(Wide_7.FxlGrp, # Wide_6, 
                    Wide_7.env, family = "negative.binomial", 
                    num.lv = 1,
                    formula = ~ SubTrt + Year.f + Rain.f + (1|Plot),
                    seed = 1234)
summary(fit_env.nb)
plot(fit_env.nb, mfrow=c(3,2))
coefplot(fit_env.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(3,4),
         order = FALSE )


fit_env.nb.PrePost <- gllvm(Wide_6,  
                            Wide_6.env, family = "negative.binomial", 
                            num.lv = 1,
                            formula = ~ Subtreatment * PrePost * Rain.f + (1|Plot),
                            seed = 1234)
summary(fit_env.nb.PrePost)
plot(fit_env.nb.PrePost, mfrow=c(3,2))
coefplot(fit_env.nb.PrePost, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(3,4),
         order = FALSE )



#make nicer plots of  fit_env.nb
#fit_env.nb <- fit_env.nb.PrePost
estimate <- data.frame(fit_env.nb[["params"]][["Xcoef"]])
estimate <- tibble::rownames_to_column(estimate, "Species")
estimate.sd <- data.frame(fit_env.nb[["sd"]][["Xcoef"]])
estimate.sd <- tibble::rownames_to_column(estimate.sd, "Species.sd")

#make df
estimate.tidy <- estimate %>% 
  pivot_longer(cols = c(-Species) , names_to = "Covariate", values_to = "Estimate")

estimate.sd.tidy <- estimate.sd %>% 
  pivot_longer(cols = c(-Species.sd) , names_to = "Covariate", values_to = "Estimate.sd")

glvvm.coef.plot <- tibble(cbind(estimate.tidy, estimate.sd.tidy[,-2]))

#filter(SpeciesCode_qaqc, FxlGrp == "NAF")
#create type field
glvvm.coef.plot$Type <- ifelse(glvvm.coef.plot$Species == "HIIN", "Summer Mustard",
                               ifelse(glvvm.coef.plot$Species == "CESO", "Yellow Star Thistle", 
                                      ifelse(glvvm.coef.plot$Species == "ESCA", "Wildflower",
                                             ifelse(glvvm.coef.plot$Species == "DELO", "Wildflower",
                                                    ifelse(glvvm.coef.plot$Species == "CAEX", "Wildflower",
                                                           ifelse(glvvm.coef.plot$Species == "LAGR", "Wildflower",
                                                                  ifelse(glvvm.coef.plot$Species == "ACAM", "Native Forb",
                                                                         ifelse(glvvm.coef.plot$Species == "AMSIN", "Native Forb", 
                                                                                ifelse(glvvm.coef.plot$Species == "CACO", "Native Forb",
                                                                                       ifelse(glvvm.coef.plot$Species == "CAME", "Native Forb",
                                                                                              ifelse(glvvm.coef.plot$Species == "CLPU", "Native Forb",
                                                                                                     ifelse(glvvm.coef.plot$Species == "CRSE", "Native Forb",
                                                                                                            ifelse(glvvm.coef.plot$Species == "EUCH", "Native Forb", 
                                                                                                                   ifelse(glvvm.coef.plot$Species == "LUBI", "Native Forb",
                                                                                                                          ifelse(glvvm.coef.plot$Species == "TRGR", "Native Forb",
                                                                                                                                 ifelse(glvvm.coef.plot$Species == "TRIFO", "Native Forb",
                                                                                                                                        ifelse(glvvm.coef.plot$Species == "CEGL", "Non-Native Forb", 
                                                                                                                                               ifelse(glvvm.coef.plot$Species == "ERBO", "Non-Native Forb",
                                                                                                                                                      ifelse(glvvm.coef.plot$Species == "ERCI", "Non-Native Forb",
                                                                                                                                                             ifelse(glvvm.coef.plot$Species == "ERMO", "Non-Native Forb",
                                                                                                                                                                    ifelse(glvvm.coef.plot$Species == "LAAM", "Non-Native Forb",
                                                                                                                                                                           ifelse(glvvm.coef.plot$Species == "MEPO", "Non-Native Forb",
                                                                                                                                                                                  ifelse(glvvm.coef.plot$Species == "STME", "Non-Native Forb",
                                                                                                                                                                                         ifelse(glvvm.coef.plot$Species == "VEPE", "Non-Native Forb",
                                                                                                                                                                                                ifelse(glvvm.coef.plot$Species == "BareGround", "BareGround",
                                                                                                                                                                                                       ifelse(glvvm.coef.plot$Species == "DeadSum", "DeadSum",
                                                                                                                                                                                                "Other"))))))))
                                                                                                                                 ))))))))))))))))))

# Forest PLot  
#remove the XXX species
glvvm.coef.plot %>% 
  filter(Species != "XXX") %>%

ggplot(aes(reorder(Type, -Estimate), Estimate, color = Type)) +
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(NULL) + 
  theme_classic(base_size = 16) +
  facet_wrap(.~Covariate)

ggsave("Output/glvvmPlot.jpg", width = 30, height = 20, units = "cm")



# Forest Plot by species
glvvm.coef.plot %>% 
  filter(Species != "XXX") %>%

ggplot(aes(reorder(Species, -Estimate), Estimate, color = Type)) +
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab(NULL) + 
  theme_gray(base_size = 10) +
  facet_wrap(.~Covariate)

ggsave("Output/glvvmPlotBySpecies.jpg", width = 30, height = 30, units = "cm")





#correlation plots
# Residual correlation matrix:
par(mfrow=c(1,1))
#for model with NO  covariates
cr <- getResidualCor(fit_ord)
library(corrplot)
library(gclus)
#> corrplot 0.94 loaded
#> Loading required package: cluster
corrplot(cr[order.single(cr), order.single(cr)], diag = FALSE, type = "lower", 
         method = "square", tl.cex = 0.5, tl.srt = 45, tl.col = "red")


