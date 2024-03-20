#### START UP ##################################################################

library(tidyverse)
library(sf)
library(qs)
library(future)
plan(multisession)
library(progressr)
handlers(global = TRUE)

# remotes::install_github("curbcut/curbcut")
# remotes::install_github("curbcut/cc.data")

col_palette <- c("#d55e00", "#cc79a7", "#0072b2", "#f0e442", "#009e73")