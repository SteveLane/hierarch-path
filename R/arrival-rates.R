################################################################################
################################################################################
## Title: Arrival Rates
## Author: Steve Lane
## Date: Friday, 16 November 2018
## Synopsis: Functions for generating simulated arrival and detection data.
## Time-stamp: <2018-11-19 09:37:02 (slane)>
################################################################################
################################################################################
#' Generates arrival rates, depending on the type of model specified.
#' 
#' @param df data frame. Data frame to hold the arrivals data.
#' @param T Integer. Number of time periods.
#' @param S Integer. Number of sites.
#' @param model Character. Character value determining arrival model.
arrivals <- function(T, S, model){
    df <- expand.grid(
        site = seq_len(S),
        time = seq_len(T)
    ) %>%
        as_data_frame()
    switch(model,
        constant = {
            df <- df %>%
                mutate(N = 50)
        },
        linear1 = {
            df <- df %>%
                mutate(N = 5)
        }
    )
    df
}

################################################################################
#' Function to calculate arrival rate as a linear combination of time.
#'
#' @param t integer. Time input.
