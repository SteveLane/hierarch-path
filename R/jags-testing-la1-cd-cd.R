################################################################################
################################################################################
## Title: JAGS Testing
## Author: Steve Lane
## Date: Tuesday, 04 December 2018
## Synopsis: This script tests the linear arrival, constant detection,
## constant interior detection JAGS model on simulated data.
## Time-stamp: <2018-12-14 10:34:24 (slane)>
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
la1_cd_cd_data <- sim_detections_interior(
    arrival_model = linear_arrival1,
    detection_model = constant_detection,
    interior_detection_model = constant_detection_interior,
    T = 10,
    S = 10,
    r = 1,
    D = 75,
    p = 0.6,
    p_interior = 0.4
)
## fit the model using jags
## this version uses pretty noninformative priors for the detection
## probabilities.
jags.data <- list(
    n = nrow(la1_cd_cd_data),
    count = la1_cd_cd_data[["D"]],
    interior_count = la1_cd_cd_data[["D_interior"]],
    x = la1_cd_cd_data[["xvar_arrival"]],
    p_alpha = 2, p_beta = 2,
    p_alpha_interior = 2, p_beta_interior = 2
)

la1_cd_cd_fit <- jags(
    data = jags.data,
    parameters.to.save =
        c("p", "N", "p_interior", "alpha_arrival", "beta_arrival"),
    model.file = here(
        "scripts/linear-arrival-constant-detection-constant-interior.jag"
    ),
    n.chains = 4, n.thin = 5, n.iter = 100000, n.burnin = 50000,
    parallel = TRUE, n.cores = 4
)

la1_cd_cd_samples <- as.matrix(la1_cd_cd_fit$samples, chains = TRUE) %>%
    as_tibble() %>%
    rename(Chain = CHAIN)
## plot a selection of posterior estimates of N
## plot a selection of posterior estimates
inds <- sample(1:100, 9)
pars <- paste0("N[", inds, "]")
Nsamps <- la1_cd_cd_samples %>%
    select(pars) %>%
    gather(site, N)
Nact <- tibble(
    site = pars,
    N = la1_cd_cd_data[["N"]][inds]
)
nplot <- seq(floor(min(Nsamps[["N"]])*0.8), ceiling(max(Nsamps[["N"]])*1.25))
Ndens <- tibble(
    N = rep(nplot, 9),
    site = rep(pars, each = length(nplot)),
    dens = c(sapply(
        la1_cd_cd_data[["log_rate"]][inds],
        function(l) dpois(nplot, exp(l))
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

pdf(file = here("figs/jags-testing/la1-cd-cd-tests.pdf"))
pl_N
mcmc_hist(la1_cd_cd_samples, pars = "alpha_arrival") +
    geom_vline(xintercept = 3, col = "maroon")
mcmc_hist(la1_cd_cd_samples, pars = "beta_arrival") +
    geom_vline(xintercept = 0.1, col = "maroon")
mcmc_hist(la1_cd_cd_samples, pars = "p") +
    geom_vline(xintercept = 0.6, col = "maroon")
mcmc_hist(la1_cd_cd_samples, pars = "p_interior") +
    geom_vline(xintercept = 0.4, col = "maroon")
dev.off()

## Samples
summaries <- la1_cd_cd_samples %>%
    gather(param, value, -Chain) %>%
    group_by(param) %>%
    summarise_at(vars(value),
                 funs(ll1 = quantile(., probs = 0.055),
                      ll2 = quantile(., probs = 0.25),
                      med = quantile(., probs = 0.5),
                      ul2 = quantile(., probs = 0.75),
                      ul1 = quantile(., probs = 0.945)))
summaries %>%
    filter(param %in% c("p", "p_interior", "alpha_arrival", "beta_arrival"))
