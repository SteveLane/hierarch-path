################################################################################
################################################################################
## Title: JAGS Testing
## Author: Steve Lane
## Date: Tuesday, 04 December 2018
## Synopsis: This script tests the constant arrival, linear detection (with
## site-specific effects) JAGS model on simulated data.
## Time-stamp: <2018-12-04 09:26:17 (slane)>
################################################################################
################################################################################
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(jagsUI)
library(ggplot2)
library(bayesplot)
source(here("R/arrival-rates.R"))
source(here("R/detection-rates.R"))

################################################################################
## Constant arrival, linear detection with site-specific effects
ca_ld3_data <- sim_detections(
    arrival_model = constant_arrival,
    detection_model = linear_detection3,
    T = 10,
    S = 10,
    r = 1,
    D = 75
)
## fit the model using jags
jags.data <- list(
    n = nrow(ca_ld3_data), count = ca_ld3_data[["D"]], hyper = 90,
    x = ca_ld3_data[["xvar_detection"]], site = ca_ld3_data[["site"]],
    nsite = max(ca_ld3_data[["site"]])
)
ca_ld3_fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N", "alpha", "beta", "sigma.eta"),
    model.file =
        here("scripts/constant-arrival-linear-detection-site-specific.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
ca_ld3_samples <- as.matrix(ca_ld3_fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)
## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
pdf(file = here("figs/jags-testing/ca-ld3-tests.pdf"))
mcmc_hist(ca_ld3_samples, pars = pars) +
    geom_vline(xintercept = 75, col = "maroon")
mcmc_hist(ca_ld3_samples, pars = "lambda") +
    geom_vline(xintercept = 75, col = "maroon")
mcmc_hist(ca_ld3_samples, pars = "alpha") +
    geom_vline(xintercept = -1, col = "maroon")
mcmc_hist(ca_ld3_samples, pars = "beta") +
    geom_vline(xintercept = 1, col = "maroon")
mcmc_hist(ca_ld3_samples, pars = "sigma.eta") +
    geom_vline(xintercept = 0.5, col = "maroon")
dev.off()
