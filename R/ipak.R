#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
################################################################################
## Title: ipak
## Author: Steve Lane
## Synopsis: Script to test for installed packages, and if not installed,
## install them.
################################################################################
################################################################################
if(length(args) == 0){
    stop("A single argument must be passed to ipak.R: insts.\ninsts is the location of a newline separated list of required packages:\n\tRscript ipak.R insts=path/to/installs.txt\nA repository may be passed using the repos option:\n\tRscript ipak.R insts=path/to/installs.txt repos=REPOS",
         call. = FALSE)
} else {
    hasOpt <- grepl("=", args)
    argLocal <- strsplit(args[hasOpt], "=")
    for(i in seq_along(argLocal)){
        value <- NA
        tryCatch(value <- as.double(argLocal[[i]][2]), warning = function(e){})
        if(!is.na(value)){
            ## Assume int/double
            assign(argLocal[[i]][1], value, inherits = TRUE)
        } else {
            assign(argLocal[[i]][1], argLocal[[i]][2], inherits = TRUE)
        }
    }
}
if(exists("repos")){
    options(repos = c(CRAN = repos))
} else {
    ## Set default cran repository.
    ## Use the one set from .Rprofile if given, else set.
    if(getOption("repos") == "@CRAN@" | is.null(getOption("repos"))){
        options(repos = c(CRAN = "https://cran.rstudio.com"))
    }
}   
## Check if packages installed
pkg <- scan(insts, "character")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
## Install if not already installed (including dependencies).
if(length(new.pkg) > 0){
    install.packages(new.pkg, dependencies = TRUE)
} else {
    message("All packages currently installed.")
}
