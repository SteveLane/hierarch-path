################################################################################
################################################################################
## Title: JAGS Testing
## Author: Steve Lane
## Date: Tuesday, 04 December 2018
## Synopsis: This script tests the linear arrival, constant detection JAGS
## model on simulated data.
## Time-stamp: <2018-12-04 12:03:16 (slane)>
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
la1_cd_data <- sim_detections(
    arrival_model = linear_arrival1,
    detection_model = constant_detection,
    T = 10,
    S = 10,
    r = 1,
    D = 75,
    p = 0.7
)
## fit the model using jags
jags.data <- list(
    n = nrow(la1_cd_data), count = la1_cd_data[["D"]],
    x = la1_cd_data[["xvar_arrival"]]
)
la1_cd_fit <- jags(
    data = jags.data,
    parameters.to.save = c("p", "N", "alpha_arrival", "beta_arrival"),
    model.file = here("scripts/linear-arrival-constant-detection.jag"),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)
la1_cd_samples <- as.matrix(la1_cd_fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)
## plot a selection of posterior estimates
inds <- sample(1:100, 9)
pars <- paste0("N[", inds, "]")
Nsamps <- la1_cd_samples %>%
    select(pars) %>%
    gather(site, N)
Nact <- tibble(
    site = pars,
    N = la1_cd_data[["N"]][inds]
)
nplot <- seq(floor(min(Nsamps[["N"]])*0.8), ceiling(max(Nsamps[["N"]])*1.25))
Ndens <- tibble(
    N = rep(nplot, 9),
    site = rep(pars, each = length(nplot)),
    dens = c(sapply(
        la1_cd_data[["N"]][inds],
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
pdf(file = here("figs/jags-testing/la1-cd-tests.pdf"))
pl_N
mcmc_hist(la1_cd_samples, pars = "p") +
    geom_vline(xintercept = 0.7, col = "maroon")
mcmc_hist(la1_cd_samples, pars = "alpha_arrival") +
    geom_vline(xintercept = 10, col = "maroon")
mcmc_hist(la1_cd_samples, pars = "beta_arrival") +
    geom_vline(xintercept = 10, col = "maroon")
dev.off()
## Samples
summaries <- la1_cd_samples %>%
    gather(param, value, -Chain) %>%
    group_by(param) %>%
    summarise_at(vars(value),
                 funs(ll1 = quantile(., probs = 0.055),
                      ll2 = quantile(., probs = 0.25),
                      med = quantile(., probs = 0.5),
                      ul2 = quantile(., probs = 0.75),
                      ul1 = quantile(., probs = 0.945)))
