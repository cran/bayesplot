## ---- settings, include=FALSE--------------------------------------------
library("bayesplot")
library("ggplot2")
library("gridExtra")
library("rstan")
knitr::opts_chunk$set(
  dev = "pdf",
  fig.align = "center",
  fig.width = 4,
  fig.height = 3,
  comment = NA
)

## ---- eval=FALSE---------------------------------------------------------
#  library("bayesplot")
#  library("ggplot2")
#  library("gridExtra")
#  library("rstan")

## ---- schools_dat--------------------------------------------------------
schools_dat <- list(
  J = 8, 
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

## ---- compile-models, eval=FALSE-----------------------------------------
#  schools_mod1 <- stan_model("schools_mod1.stan")
#  schools_mod2 <- stan_model("schools_mod2.stan")

## ---- fit-models-hidden, results='hide', message=FALSE-------------------
fit1 <- sampling(schools_mod1, data = schools_dat)
fit2 <- sampling(schools_mod2, data = schools_dat)

## ---- extract-draws------------------------------------------------------
# Extract posterior draws for later use
posterior1 <- as.array(fit1)
posterior2 <- as.array(fit2)

## ---- results='hide'-----------------------------------------------------
fit1_50iter <- sampling(schools_mod1, data = schools_dat, chains = 2, iter = 50)

## ----print-rhats---------------------------------------------------------
library("bayesplot")
rhats <- rhat(fit1_50iter)
print(rhats)

## ----mcmc_rhat-----------------------------------------------------------
color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(rhats)

## ---- mcmc_rhat-2--------------------------------------------------------
mcmc_rhat(rhats) + yaxis_text()

## ---- results='hide'-----------------------------------------------------
mcmc_rhat(rhat = rhat(fit1)) + yaxis_text()

## ----print-neff-ratios---------------------------------------------------
ratios1 <- neff_ratio(fit1)
print(ratios1)
mcmc_neff(ratios1)

## ----mcmc_neff-compare---------------------------------------------------
# A function we'll use several times to plot comparisons of the centered
# parameterization (cp) and the non-centered parameterization (ncp)
compare_cp_ncp <- function(cp_plot, ncp_plot, ...) {
  grid.arrange(
    cp_plot + labs(subtitle = "Centered parameterization"),
    ncp_plot + labs(subtitle = "Non-centered parameterization"), 
    ...
  )
}

neff1 <- neff_ratio(fit1, pars = c("theta", "mu", "tau"))
neff2 <- neff_ratio(fit2, pars = c("theta", "mu", "tau"))
compare_cp_ncp(mcmc_neff(neff1), mcmc_neff(neff2))

## ----mcmc_acf------------------------------------------------------------
grid.arrange(
  mcmc_acf(posterior1, pars = "theta[1]", lags = 10),
  mcmc_acf(posterior2, pars = "eta[1]", lags = 10),
  ncol = 2
)

## ---- extract-nuts-info--------------------------------------------------
lp1 <- log_posterior(fit1)
head(lp1)
np1 <- nuts_params(fit1)
head(np1)

# for the second model
lp2 <- log_posterior(fit2)
np2 <- nuts_params(fit2)

## ---- mcmc_nuts_divergence-----------------------------------------------
color_scheme_set("red")
mcmc_nuts_divergence(np1, lp1)

## ---- mcmc_nuts_divergence-chain-----------------------------------------
mcmc_nuts_divergence(np1, lp1, chain = 4)

## ---- mcmc_nuts_divergence-2---------------------------------------------
mcmc_nuts_divergence(np2, lp2)

## ---- fit-adapt-delta, results='hide',message=FALSE----------------------
fit1b <- sampling(schools_mod1, data = schools_dat,
                  control = list(adapt_delta = 0.99))
fit2b <- sampling(schools_mod2, data = schools_dat,
                  control = list(adapt_delta = 0.99))

## ---- mcmc_nuts_divergence-3---------------------------------------------
mcmc_nuts_divergence(nuts_params(fit1b), log_posterior(fit1b))
mcmc_nuts_divergence(nuts_params(fit2b), log_posterior(fit2b))

## ---- mcmc_nuts_energy-1, message=FALSE----------------------------------
mcmc_nuts_energy(np1)

## ---- mcmc_nuts_energy-2-------------------------------------------------
mcmc_nuts_energy(np1, merge_chains = FALSE, binwidth = 1)

## ---- mcmc_nuts_energy-3, message=FALSE----------------------------------
compare_cp_ncp(
  mcmc_nuts_energy(np1, binwidth = 1/2),
  mcmc_nuts_energy(np2, binwidth = 1/2)
)

## ---- mcmc_nuts_energy-4, message=FALSE----------------------------------
np1b <- nuts_params(fit1b)
np2b <- nuts_params(fit2b)
compare_cp_ncp(
  mcmc_nuts_energy(np1b), 
  mcmc_nuts_energy(np2b)
)

