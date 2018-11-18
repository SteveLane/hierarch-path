################################################################################
################################################################################
## Title: Test Bed
## Author: Steve Lane
## Date: Monday, 19 November 2018
## Synopsis: Script to test out functions etc.
## Time-stamp: <2018-11-19 09:35:27 (slane)>
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

## Generate initial site arrivals data.
arrival_data <- arrivals(10, 10, "constant")

################################################################################
## Generate detections data (constant model initially)
detections <- arrival_data %>%
    mutate(
        count = map_int(N, rbinom, p = 0.6, n = 1)
    )

## fit the model using jags
jags.data <- list(n = nrow(detections), count = detections[["count"]])
fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N"),
    model.file = here("scripts/constant.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
samples <- as.matrix(fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)

## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
mcmc_hist(samples, pars = pars)
mcmc_hist(samples, pars = c("lambda", "p"))

## and summarise
summaries <- samples %>%
    gather(param, value, -Chain) %>%
    group_by(param) %>%
    summarise_at(vars(value),
                 funs(ll1 = quantile(., probs = 0.055),
                      ll2 = quantile(., probs = 0.25),
                      med = quantile(., probs = 0.5),
                      ul2 = quantile(., probs = 0.75),
                      ul1 = quantile(., probs = 0.945)))

################################################################################
## Generate detections with increasing rate of detection.
detections <- arrival_data %>%
    mutate(
        p = plogis(-0.5 + 0.1 * time),
        count = map2_int(N, p, rbinom, n = 1)
    )

## fit the model using jags
jags.data <- list(n = nrow(detections), count = detections[["count"]],
                  time = detections[["time"]])
fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N"),
    model.file = here("scripts/constant.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
samples <- as.matrix(fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)

## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
mcmc_hist(samples, pars = pars)
mcmc_hist(samples, pars = c("lambda", "p"))

## and summarise
summaries <- samples %>%
    gather(param, value, -Chain) %>%
    group_by(param) %>%
    summarise_at(vars(value),
                 funs(ll1 = quantile(., probs = 0.055),
                      ll2 = quantile(., probs = 0.25),
                      med = quantile(., probs = 0.5),
                      ul2 = quantile(., probs = 0.75),
                      ul1 = quantile(., probs = 0.945)))
