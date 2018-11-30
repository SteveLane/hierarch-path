################################################################################
################################################################################
## Title: Arrival Rates
## Author: Steve Lane
## Date: Friday, 16 November 2018
## Synopsis: Functions for generating simulated arrival and detection data.
## Time-stamp: <2018-11-30 14:34:58 (slane)>
################################################################################
################################################################################
#' Generates arrival rates, depending on the type of model specified.
#' 
#' @param T Integer. Number of time periods.
#' @param S Integer. Number of sites.
#' @param model function name. Passed as an unquoted variable.
#' @param D integer. Arrival rate for constant model.
arrivals <- function(T, S, model, D = 50){
    df <- expand.grid(
        site = seq_len(S),
        time = seq_len(T)
    ) %>%
        dplyr::as_data_frame()
    model_nm <- enquo(model)
    df <- df %>%
        dplyr::group_by(site) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            df = purrr::map(data, !!model_nm, D)
        ) %>%
        tidyr::unnest()
    df
}

################################################################################
#' Function to calculate arrival rate as a constant.
#'
#' This function is required so that the call may be made using tidy eval.
#'
#' @param df data frame.
#' @param D integer. Arrival rate for constant model. Defaults to 50.
#' 
#' @return dataframe containing the arrival rate as N = D.
constant_arrival <- function(df, D = 50) {
n <- nrow(df)
    dplyr::data_frame(
        N = rep(D, n)
    )
}

################################################################################
#' Function to calculate arrival rate as a linear combination of time.
#'
#' @param df data frame. Must contain a \code{time} column.
#' 
#' @return dataframe containing the X variable as X = 5 + N(0.25t, 1) and the
#'     arrival rate as Poisson(10 + 10X). The (true) underlying arrival rate is
#'     also returned.
linear_arrival1 <- function(df, ...) {
    time <- df[["time"]]
    n <- length(time)
    xvar <- 5 + rnorm(n, mean = 0.25 * time, sd = 1)
    rate <- 10 + 10*xvar
    n_arrival <- rpois(n, rate)
    dplyr::data_frame(
        xvar_arrival = xvar, N = n_arrival, rate = rate
    )
}

################################################################################
#' Function to calculate arrival rate dependent on time and site effects.
#'
#' @param df data frame. Must contain a \code{time} column.
#' 
#' @return dataframe containing the X variable as X = 5 + N(0.25t, 1) and the
#'     arrival rate as Poisson(10 + 10X + site), where site is N(0, 3). The
#'     (true) underlying arrival rate is also returned. 
linear_arrival2 <- function(df, ...) {
    time <- df[["time"]]
    site_effect <- rnorm(1, 0, 3)
    n <- length(time)
    xvar <- 5 + rnorm(n, mean = 0.25 * time, sd = 1)
    rate <- 10 + 10*xvar + site_effect
    n_arrival <- rpois(n, rate)
    dplyr::data_frame(
        xvar_arrival = xvar, N = n_arrival, rate = rate,
        site_effect = site_effect
    )
}

################################################################################
#' Function to calculate arrival rate dependent on time and site effects.
#'
#' @param df data frame. Must contain a \code{time} column.
#' 
#' @return dataframe containing the X variable as X = 5 + N(0.25t, 1) and the
#'     arrival rate as Poisson(10 + (10 + site_slope)X + site), where site is
#'     N(0, 3) and site_slope is N(0, 1). The (true) underlying arrival rate is
#'     also returned.
linear_arrival3 <- function(df, ...) {
    time <- df[["time"]]
    site_effect <- rnorm(1, 0, 3)
    site_slope <- rnorm(1, 0, 1)
    n <- length(time)
    xvar <- 5 + rnorm(n, mean = 0.25 * time, sd = 1)
    rate <- 10 + (10 + site_slope)*xvar + site_effect
    n_arrival <- rpois(n, rate)
    dplyr::data_frame(
        xvar_arrival = xvar, N = n_arrival, rate = rate,
        site_effect = site_effect, site_slope = site_slope
    )
}

################################################################################
#' Simulates r replicates of arrival rates.
#'
#' @param model function name. Passed as an unquoted variable.
#' @param T Integer. Number of time periods.
#' @param S Integer. Number of sites.
#' @param r integer. Number of replicates for simulation.
#' @param D integer. Arrival rate for constant model.
#'
#' @return data frame consisting of simulation replicates.
sim_arrivals <- function(model, T, S, r, D = 50) {
    df <- replicate(
        n = r,
        expr = arrivals(T = T, S = S, model = model, D),
        simplify = FALSE
    )
    df <- bind_rows(df, .id = "replicate")
    df
}
