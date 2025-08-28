library(brms)

t0 <- Sys.time()


fit1 <- brm(
  count ~ zBase * Trt + (1|patient),
  data = epilepsy, family = poisson(),
  prior = prior(normal(0, 10), class = b) +
    prior(cauchy(0, 2), class = sd)
)

summary(fit1)


t1 <- Sys.time()
runtime <- t1-t0
runtime


# fit2 <- brm(rating ~ period + carry + cs(treat),
#             data = inhaler, family = sratio("logit"),
#             prior = set_prior("normal(0,5)"), chains = 2)
# 
# summary(fit2)
# 
# 
# 
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::install_cmdstan()
