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


## JWM: Where is N defined?
# Model with b2 stars and b1 degree
degreedist <- table(degree(N))

## What is JSF? I can't pull dollarsobligated out of it:
## > JSF$dollarsobligated
## Error in JSF$dollarsobligated : $ operator is invalid for atomic vectors
attr <- as.list(as.numeric(JSF$dollarsobligated))

set.vertex.attribute(N, names(attr), attr)

## JWM: What is an empty edgecov?
mod4 <- ergm(N ~ edges + b1degree(2:4) + b2star(3) + edgecov(""))


## -----------------------------------------------------------------------------
## JWM: Merge the all the JSF networks, look at the network

fix_adj <- function(adj)
{
    rownames(adj) <- adj[,1]
    adj[,-1]
}

Adj <- lapply(JSFadj, fix_adj)
FullNet <- network(Reduce(`+`, Adj), directed=FALSE, bipartite=TRUE)

## JWM: Why do we have isolates? A CD or agency can't get into the data set if
## it is an isolate, right?
plot(FullNet)
