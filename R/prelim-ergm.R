## =============================================================================
## Bipartite ERGMs on JSF Networks
## =============================================================================

## Load libraries
require(data.table)
require(network)
require(ergm)
require(texreg)

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
    MCMLE.maxit=25)

## summary(FullNet ~ b1degree(1:10))
## summary(FullNet ~ b2star(2:10))

## Model 0
mod0 <- ergm(FullNet ~ edges, control=ctrl, verbose = TRUE)
mod0_diag <- mcmc.diagnostics(mod0)
gof0 <- gof(mod0)
par(mfrow=c(2,2)); plot(gof0)
summary(mod0)

## Model 1
mod1 <- ergm(FullNet ~ edges
             + gwb1degree(0.5, fixed=TRUE)
             ## + gwb2degree(0, fixed=TRUE),
             + b2star(2:4),
             control=ctrl, verbose=TRUE)

mod1_diag <- mcmc.diagnostics(mod1)
gof1 <- gof(mod1)
par(mfrow=c(2,2)); plot(gof1)
summary(mod1)
plotreg(mod1)



## Model 2
mod2 <- ergm(FullNet ~ edges
             + gwb1degree(0.2, fixed=TRUE)
             + gwb2degree(0, fixed=TRUE),
             + b2star(2:4),
             + edgecov("dollarsobligated"),
             control=ctrl, verbose=TRUE)

mod2_diag <- mcmc.diagnostics(mod2)
gof2 <- gof(mod2)
par(mfrow=c(2,2)); plot(gof2)
summary(mod2)

