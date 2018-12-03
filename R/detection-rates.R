################################################################################
################################################################################
## Title: Detection Rates
## Author: Steve Lane
## Date: Tuesday, 27 November 2018
## Synopsis: Function definitions for generating simulated detection rates.
## Time-stamp: <2018-12-03 11:25:12 (slane)>
################################################################################
#' Generates detection rates
#'
#' \code{constant_detection} simulates detections at a constant rate. Must
#' contain the true number arriving, N.
#'
#' @param df data frame. Holds the arrival rate data.
#' @param p numeric. Constant detection rate.
#'
#' @return dataframe containing the number of detections, D = Binomial(N, p) and
#'     the underlying detection probability p.
constant_detection <- function(df, p = 0.6) {
    N <- df[["N"]]
    n <- length(N)
    D <- rbinom(n, N, p)
    dplyr::data_frame(
        p = p, D = D
    )
}

################################################################################
#' Generates detection rates
#'
#' \code{linear_detection1} calculates detections as a linear function of a
#' time-varying covariate (independent to that used in the arrival rate).
#'
#' @param df data frame. Holds the arrival rate data. Must contain a \code{N}
#'     column.
#'
#' @return dataframe containing the number of detections, D = Binomial(N, p) the
#'     underlying detection probability p, and the predictor xvar_detection.
#'     Where p = logit(-1 + X), X = Normal(0.1 * time, 0.1).
linear_detection1 <- function(df, ...) {
    time <- df[["time"]]
    N <- df[["N"]]
    n <- length(N)
    X <- rnorm(n, mean = 0.1 * time, sd = 0.1)
    p <- plogis(-1 + X)
    D <- rbinom(n, N, p)
    dplyr::data_frame(
        xvar_detection = X, p = p, D = D
    )
}

################################################################################
#' Generates detection rates
#'
#' \code{linear_detection2} calculates detections as a linear function of the
#' same covariate that is used in the arrival rate. Note:
#' \code{linear_detection2} can only be used in conjuction with non-constant
#' arrivals due to the presence of the covariate term.
#'
#' @param df data frame. Holds the arrival rate data. Must contain a
#'     \code{xvar_arrival} and \code{N} column.
#'
#' @return dataframe containing the number of detections, D = Binomial(N, p) and
#'     the underlying detection probability p.
#'     Where p = logit(-3 + 0.6 * xvar_arrival).
linear_detection2 <- function(df, ...) {
    N <- df[["N"]]
    n <- length(N)
    X <- df[["xvar_arrival"]]
    p <- plogis(-3 + 0.6 * X)
    D <- rbinom(n, N, p)
    dplyr::data_frame(
        p = p, D = D
    )
}

################################################################################
#' Generates detection rates
#'
#' \code{linear_detection3} calculates detections as a linear function of a
#' time-varying covariate (independent to that used in the arrival rate), and a
#' site-specific intercept.
#'
#' @param df data frame. Holds the arrival rate data. Must contain a \code{N}
#'     column.
#'
#' @return dataframe containing the number of detections, D = Binomial(N, p) the
#'     underlying detection probability p, and the predictor xvar_detection.
#'     Where p = logit(-1 + X + site_effect), X = Normal(0.1 * time, 0.1),
#'     site_effect = Normal(0, 0.5).
linear_detection3 <- function(df, ...) {
    time <- df[["time"]]
    N <- df[["N"]]
    n <- length(N)
    X <- rnorm(n, mean = 0.1 * time, sd = 0.1)
    site_effect <- rnorm(1, 0, 0.5)
    p <- plogis(-1 + X + site_effect)
    D <- rbinom(n, N, p)
    dplyr::data_frame(
        xvar_detection = X, p = p, D = D, site_effect
    )
}

################################################################################
#' Generates detection rates
#'
#' \code{linear_detection4} calculates detections as a linear function of the
#' same covariate that is used in the arrival rate and site-specific intercept.
#' Note: \code{linear_detection4} can only be used in conjuction with
#' non-constant arrivals due to the presence of the covariate term.
#'
#' @param df data frame. Holds the arrival rate data. Must contain a
#'     \code{xvar_arrival} and \code{N} column.
#'
#' @return dataframe containing the number of detections, D = Binomial(N, p) and
#'     the underlying detection probability p.
#'     Where p = logit(-3 + 0.6 * xvar_arrival + site_effect) and
#'     site_effect = Normal(0, 0.5).
linear_detection4 <- function(df, ...) {
    N <- df[["N"]]
    n <- length(N)
    X <- df[["xvar_arrival"]]
    site_effect <- rnorm(1, 0, 0.5)
    p <- plogis(-3 + 0.6 * X + site_effect)
    D <- rbinom(n, N, p)
    dplyr::data_frame(
        p = p, D = D, site_effect = site_effect
    )
}

################################################################################
#' Simulates r replicates of arrival and detection rates.
#'
#' \code{sim_detections} simulates the full arrival and detection process. It
#' first calls \code{sim_arrivals} to generate the arrivals, then maps over the
#' resultant data frame to generate the detections.
#'
#' @param arrival_model function name. Passed as an unquoted variable.
#' @param detection_model function name. Passed as an unquoted variable.
#' @param T Integer. Number of time periods.
#' @param S Integer. Number of sites.
#' @param r integer. Number of replicates for simulation.
#' @param D integer. Arrival rate for constant model, defaults to 50.
#' @param p numeric. Detection rate for constant model.
#'
#' @return data frame consisting of simulation replicates.
sim_detections <- function(arrival_model, detection_model, T, S, r,
                           D = 50, p = 0.6) {
    arrive_df <- replicate(
        n = r,
        expr = arrivals(T = T, S = S, model = arrival_model, D = D),
        simplify = FALSE
    )
    arrive_df <- dplyr::bind_rows(arrive_df, .id = "replicate")
    detect_df <- dplyr::group_by(arrive_df, replicate, site)
    detect_df <- tidyr::nest(detect_df)
    detect_df <- dplyr::mutate(detect_df,
        df = purrr::map(data, detection_model, p = p)
    )
    detect_df <- tidyr::unnest(detect_df)
    detect_df
}
