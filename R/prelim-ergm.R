## =============================================================================
## Bipartite ERGMs on JSF Networks
## =============================================================================

## Load libraries
require(data.table)
require(network)
require(ergm)

## NOTE: Set working directory in R to the root of the contracts project.
## setwd("~/jwm/research/contracts")

## Load data
JSF <- load("data/JSF-networks.RData")


## -----------------------------------------------------------------------------
## Estimate ERGMs
## -----------------------------------------------------------------------------

ctrl <- control.ergm(
    ## main.method=c("Stepping"),
    ## MCMC.prop.weights="TNT",
    MCMC.interval=1000,
    MCMC.burnin=200000,
    MCMC.samplesize=100000,
    MCMLE.maxit=10)

## summary(FullNet ~ b1degree(1:10))
## summary(FullNet ~ b2star(2:10))

mod1 <- ergm(FullNet ~ edges
             + gwb1degree(0.1, fixed=TRUE)
             ## + gwb2degree(0, fixed=TRUE),
             + b2star(2:4),
             control=ctrl, verbose=TRUE)

summary(mod1)

gof(mod1)

plot(gof(mod1))
