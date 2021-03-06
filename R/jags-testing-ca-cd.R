################################################################################
################################################################################
## Title: JAGS Testing
## Author: Steve Lane
## Date: Tuesday, 04 December 2018
## Synopsis: This script tests the constant arrival, constant detection JAGS
## model on simulated data.
## Time-stamp: <2018-12-13 15:55:47 (slane)>
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
## Constant arrival, constant detection
ca_cd_data <- sim_detections(
    arrival_model = constant_arrival,
    detection_model = constant_detection,
    T = 10,
    S = 10,
    r = 1,
    D = 75,
    p = 0.7
)
## fit the model using jags
jags.data <- list(
    n = nrow(ca_cd_data), count = ca_cd_data[["D"]], hyper = 100
)
ca_cd_fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N"),
    model.file = here("scripts/constant.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
ca_cd_samples <- as.matrix(ca_cd_fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)
## plot a selection of posterior estimates of N
pdf(file = here("figs/jags-testing/ca-cd-tests.pdf"))
mcmc_hist(ca_cd_samples, pars = "N") +
    geom_vline(xintercept = 75, col = "maroon")
mcmc_hist(ca_cd_samples, pars = "lambda") +
    geom_vline(xintercept = 75, col = "maroon")
mcmc_hist(ca_cd_samples, pars = "p") +
    geom_vline(xintercept = 0.7, col = "maroon")
dev.off()
