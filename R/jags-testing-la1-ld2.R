################################################################################
################################################################################
## Title: JAGS Testing
## Author: Steve Lane
## Date: Tuesday, 04 December 2018
## Synopsis: This script tests the linear arrival, linear detection (using
## arrival covariate) JAGS model on simulated data.
## Time-stamp: <2018-12-04 15:10:39 (slane)>
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
la1_ld2_data <- sim_detections(
    arrival_model = linear_arrival1,
    detection_model = linear_detection2,
    T = 10,
    S = 10,
    r = 1,
    D = 75,
    p = 0.7
)
## fit the model using jags
jags.data <- list(
    n = nrow(la1_ld2_data), count = la1_ld2_data[["D"]],
    x_arrival = la1_ld2_data[["xvar_arrival"]],
    x_detection = la1_ld2_data[["xvar_arrival"]]
)
la1_ld2_fit <- jags(
    data = jags.data,
    parameters.to.save = c("p", "N", "alpha_arrival", "beta_arrival",
        "alpha_detection", "beta_detection"),
    model.file = here("scripts/linear-arrival-linear-detection.jag"),
    n.chains = 4, n.thin = 5, n.iter = 500000, n.burnin = 250000,
    parallel = TRUE, n.cores = 4
)
la1_ld2_samples <- as.matrix(la1_ld2_fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)
## plot a selection of posterior estimates
inds <- sample(1:100, 9)
pars <- paste0("N[", inds, "]")
Nsamps <- la1_ld2_samples %>%
    select(pars) %>%
    gather(site, N)
Nact <- tibble(
    site = pars,
    N = la1_ld2_data[["N"]][inds]
)
nplot <- seq(floor(min(Nsamps[["N"]])*0.8), ceiling(max(Nsamps[["N"]])*1.25))
Ndens <- tibble(
    N = rep(nplot, 9),
    site = rep(pars, each = length(nplot)),
    dens = c(sapply(
        la1_ld2_data[["N"]][inds],
        function(l) dpois(nplot, l)
    ))
)
pl_N <- ggplot(Nsamps, aes(x = N, y = ..density..)) +
    geom_histogram() +
    facet_wrap(~ site) +
    geom_vline(data = Nact, aes(xintercept = N), col = "maroon") +
    geom_line(data = Ndens, aes(x = N, y = dens), col = "maroon") +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        )
pdf(file = here("figs/jags-testing/la1-ld2-tests.pdf"))
pl_N
mcmc_hist(la1_ld2_samples, pars = "alpha_arrival") +
    geom_vline(xintercept = 3, col = "maroon")
mcmc_hist(la1_ld2_samples, pars = "beta_arrival") +
    geom_vline(xintercept = 0.1, col = "maroon")
mcmc_hist(la1_ld2_samples, pars = "alpha_detection") +
    geom_vline(xintercept = -3, col = "maroon")
mcmc_hist(la1_ld2_samples, pars = "beta_detection") +
    geom_vline(xintercept = 0.6, col = "maroon")
dev.off()
## Samples
summaries <- la1_ld2_samples %>%
    gather(param, value, -Chain) %>%
    group_by(param) %>%
    summarise_at(vars(value),
                 funs(ll1 = quantile(., probs = 0.055),
                      ll2 = quantile(., probs = 0.25),
                      med = quantile(., probs = 0.5),
                      ul2 = quantile(., probs = 0.75),
                      ul1 = quantile(., probs = 0.945)))
