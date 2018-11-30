################################################################################
################################################################################
## Title: Detection Rates
## Author: Steve Lane
## Date: Tuesday, 27 November 2018
## Synopsis: Function definitions for generating simulated detection rates.
## Time-stamp: <2018-11-30 12:56:31 (slane)>
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
#' \code{linear_detection2} calculates detections as a linear function of the
#' same covariate that is used in the arrival rate. Note:
#' \code{linear_detection2} can only be used in conjuction with non-constant
#' arrivals due to the presence of the covariate term.
#'
#' @param df data frame. Holds the arrival rate data. Must contain a \code{xvar}
#'     column.
#'
#' @return dataframe containing the number of detections, D = Binomial(N, p) and
#'     the underlying detection probability p. Where p = logit(-3 + 0.6 * X).
linear_detection2 <- function(df) {
    N <- df[["N"]]
    n <- length(N)
    X <- df[["xvar"]]
    p <- plogis(-3 + 0.6 * X)
    D <- rbinom(n, N, p)
    dplyr::data_frame(
        p = p, D = D
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
