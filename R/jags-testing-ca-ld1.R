################################################################################
################################################################################
## Title: JAGS Testing
## Author: Steve Lane
## Date: Tuesday, 04 December 2018
## Synopsis: This script tests the constant arrival, linear detection JAGS
## model on simulated data.
## Time-stamp: <2018-12-04 09:18:42 (slane)>
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
## Constant arrival, linear detection
ca_ld1_data <- sim_detections(
    arrival_model = constant_arrival,
    detection_model = linear_detection1,
    T = 10,
    S = 10,
    r = 1,
    D = 75
)
## fit the model using jags
jags.data <- list(
    n = nrow(ca_ld1_data), count = ca_ld1_data[["D"]], hyper = 90,
    x = ca_ld1_data[["xvar_detection"]]
)
ca_ld1_fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N", "alpha", "beta"),
    model.file = here("scripts/constant-arrival-linear-detection.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
ca_ld1_samples <- as.matrix(ca_ld1_fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)
## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
pdf(file = here("figs/jags-testing/ca-ld1-tests.pdf"))
mcmc_hist(ca_ld1_samples, pars = pars) +
    geom_vline(xintercept = 75, col = "maroon")
mcmc_hist(ca_ld1_samples, pars = "lambda") +
    geom_vline(xintercept = 75, col = "maroon")
mcmc_hist(ca_ld1_samples, pars = "alpha") +
    geom_vline(xintercept = -1, col = "maroon")
mcmc_hist(ca_ld1_samples, pars = "beta") +
    geom_vline(xintercept = 1, col = "maroon")
dev.off()
