## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Matrix Seriation
## Authors: Nicolas Frerebeau, Université Bordeaux Montaigne (France)
## Contact: nicolas.frerebeau@u-bordeaux-montainge.fr
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Clean current environment ===================================================
rm(list = ls())

## Load packages ===============================================================
library(janus)

## Set Shiny settings ==========================================================
options(shiny.maxRequestSize = 30*1024^2)
enableBookmarking(store = "server")

## Set ggplot2 theme ===========================================================
old <- ggplot2::theme_set(ggplot2::theme_bw())
