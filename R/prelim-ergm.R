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
## Preliminary ERGM batch run
## -----------------------------------------------------------------------------

ctrl <- control.ergm(
    MCMC.interval=1000,
    MCMC.burnin=200000,
    MCMC.samplesize=100000,
    MCMLE.maxit=10)

plot(summary(JSFnets[[1]] ~ b1degree(1:20)))
plot(summary(JSFnets[[1]] ~ b2degree(1:20)))

mod1 <- ergm(JSFnets[[1]] ~ edges, control=ctrl, verbose=TRUE)
summary(mod1)
plot(gof(mod1))

mod2 <- ergm(JSFnets[[1]] ~ edges + b1concurrent, control=ctrl, verbose=TRUE)
summary(mod2)

mod3 <- ergm(JSFnets[[1]] ~ edges + b2star(2), control=ctrl, verbose=TRUE)
summary(mod3)

# Model with b2 stars and b1 degree 
degreedist <- table(degree(N))

attr <- as.list(as.numeric(JSF$dollarsobligated))

set.vertex.attribute(N, names(attr), attr)

mod4 <- ergm(N ~ edges + b1degree(2:4) + b2star(3) + edgecov(""))


DECAY <- seq(1, 4, by=0.2)
RESULT <- matrix(0, ncol=2, nrow=length(DECAY))

for (i in 1:length(DECAY)) {
    decay <- DECAY[i]
    RESULT[i,] <- try(coef(ergm(JSFnets[[1]] ~ edges
                                ## + gwb1degree(3.0, fixed=TRUE)
                                + gwb2degree(decay, fixed=TRUE),
                                verbose=TRUE, control=ctrl)))
}

## > RESULT
##            [,1]       [,2]
##  [1,]  1.517632  -6.944632
##  [2,]  1.578917  -4.785641
##  [3,]  2.045375  -9.890707
##  [4,]  2.466362  -8.483947
##  [5,]  2.927742  -9.103538
##  [6,]  3.434409 -10.359861
##  [7,]  3.776188  -9.870859
##  [8,]  4.276437 -10.577873
##  [9,]  4.666221 -10.473028
## [10,]  5.176068 -10.507367
## [11,]  5.622305 -10.716358
## [12,]  6.284993 -11.065329
## [13,]  7.024273 -11.621657
## [14,]  7.950342 -12.408078
## [15,]  9.117513 -13.468616
## [16,] 10.530569 -14.883984
