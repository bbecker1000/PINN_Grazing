

PCGLLVM <- gllvm(Y, X = X, family = "poisson", num.lv.c = 2, 
                 lv.formula = ~bare.sand + fallen.leaves + moss+herb.layer + reflection, 
                 formula = ~soil.dry)



fit_env.nb <- gllvm(Wide_7.FxlGrp, # Wide_6, 
                    Wide_7.env, family = "negative.binomial", 
                    num.lv = 1,
                    formula = ~ SubTrt + Year.f + Rain.f + (1|Plot),
                    seed = 1234)


fit_env.pc <- gllvm(Wide_7.FxlGrp, 
                    Wide_7.env, 
                    family = "negative.binomial", 
                    num.lv.c = 2,
                    lv.formula = ~ SubTrt + Year.f + Rain.f + (1|Plot),
                    #formula = ~ Plot, #Partial out the plot random effect
                    seed = 1234)

summary(fit_env.pc)
ordiplot(fit_env.pc, biplot = TRUE)
