################################################################################
################################################################################
## Title: Test Bed
## Author: Steve Lane
## Date: Monday, 19 November 2018
## Synopsis: Script to test out functions etc.
## Time-stamp: <2018-12-03 11:10:48 (slane)>
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

## Can generate a single arrivals set:
arrival_data <- arrivals(10, 10, constant_arrival)

## Or simulations:
sim_arrival_data <- sim_arrivals(constant_arrival, 10, 10, 100)

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

## fit the model using jags, we'll use two priors here, to show that the model
## is unidentified with the current set up.
## First, we'll put a Gamma(50, 1) prior on the arrival rate. This will give us
## a good answer as the prior is concentrated on the true arrival rate.
jags.data <- list(n = nrow(detections), count = detections[["count"]],
                  time = detections[["time"]], hyper = 50)
fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N", "alpha", "beta"),
    model.file = here("scripts/linear-time.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
samples <- as.matrix(fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)

## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
mcmc_hist(samples, pars = pars)
## Plot regression parameters
mcmc_hist(samples, pars = c("lambda", "alpha", "beta"))

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

## Now we'll put a Gamma(100, 1) prior on the arrival rate. This will give us
## a bad answer as the prior is concentrated away from the true arrival rate.
jags.data <- list(n = nrow(detections), count = detections[["count"]],
                  time = detections[["time"]], hyper = 100)
fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N", "alpha", "beta"),
    model.file = here("scripts/linear-time.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
samples <- as.matrix(fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)

## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
mcmc_hist(samples, pars = pars)
## Plot regression parameters
mcmc_hist(samples, pars = c("lambda", "alpha", "beta"))

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

## Upshot is that the posterior summaries of parameters of interest are being
## entirely driven by the choice of prior on N. I.e., we're unidentified. So, is
## there a way of getting around this?

################################################################################
## Test out detection functions
source(here("R/detection-rates.R"))

arrival_data <- arrivals(10, 10, linear_arrival1)

detections_data <- arrival_data %>%
    group_by(site) %>%
    nest() %>%
    mutate(df = map(data, constant_detection)) %>%
    unnest()

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

################################################################################
## Generate detections data (assuming arrivals is already generated).
detections_data <- arrival_data %>%
    group_by(site) %>%
    nest() %>%
    mutate(df = map(data, linear_detection1)) %>%
    unnest()

## This little snippet shows how to generate the data for simulations.
## detections_data <- sim_detections(
##     linear_arrival2, constant_detection,
##     T = 5, S = 5, r = 10
## )

################################################################################
## Now let's look at a similar model fitting exercise as above, but where we
## have constant arrival and linear detection with an X variable.
arrival_data <- arrivals(T = 10, S = 10, constant_arrival, D = 50)
detections_data <- arrival_data %>%
    group_by(site) %>%
    nest() %>%
    mutate(df = map(data, linear_detection1)) %>%
    unnest()

## First we'll put a Gamma(50, 1) prior on the arrival rate. This will give us
## a good answer as the prior is concentrated on the true arrival rate.
jags.data <- list(
    n = nrow(detections_data), count = detections_data[["D"]],
    time = detections_data[["xvar_detection"]], hyper = 50
)
fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N", "alpha", "beta"),
    model.file = here("scripts/linear-time.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
samples <- as.matrix(fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)

## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
mcmc_hist(samples, pars = pars)
## Plot regression parameters
mcmc_hist(samples, pars = c("lambda", "alpha", "beta"))

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

## Now we'll put a Gamma(80, 1) prior on the arrival rate. This probably won't
## work, as the detection probability coefficients will just adjust themselves
## to the expected rate of the prior.
jags.data <- list(
    n = nrow(detections_data), count = detections_data[["D"]],
    time = detections_data[["xvar_detection"]], hyper = 80
)
fit <- jags(
    data = jags.data,
    parameters.to.save = c("lambda", "p", "N", "alpha", "beta"),
    model.file = here("scripts/linear-time.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
samples <- as.matrix(fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)

## plot a selection of posterior estimates of N
pars <- paste0("N[", sample(1:100, 9), "]")
mcmc_hist(samples, pars = pars)
## Plot regression parameters
mcmc_hist(samples, pars = c("lambda", "alpha", "beta"))

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
