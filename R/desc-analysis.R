## =============================================================================
## Descriptive Analysis of JSF Networks
## =============================================================================

## Load libraries
require(data.table)
require(network)
require(ergm)

## NOTE: Set working directory in R to the root of the contracts project.
## setwd("~/jwm/research/contracts")

## Load data
JSF <- load("data/JSF-networks.RData")
