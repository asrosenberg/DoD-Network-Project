#______________________________________________________________________________
#______________________________________________________________________________
# Let's add a vector of 0s for each of our time slices
#______________________________________________________________________________
#______________________________________________________________________________
library(network)

setwd("~/Dropbox/DoD/data")
load("relevant_cd.RData")
load("relevant_agencies.RData")
load("final-time-slices.RData")

# We need to see if the network package can auto add nodes to our bipartite net
add.vertices(final_JSFnets[[1]], nv = 16, last.mode = FALSE)
add.vertices(final_JSFnets[[2]], nv = 16, last.mode = FALSE)
add.vertices(final_JSFnets[[3]], nv = 16, last.mode = FALSE)
add.vertices(final_JSFnets[[4]], nv = 16, last.mode = FALSE)
add.vertices(final_JSFnets[[5]], nv = 16, last.mode = FALSE)
add.vertices(final_JSFnets[[6]], nv = 16, last.mode = FALSE)
add.vertices(final_JSFnets[[7]], nv = 16, last.mode = FALSE)

add.vertices(final_JSFnets[[1]], nv = 335, last.mode = TRUE)
add.vertices(final_JSFnets[[2]], nv = 335, last.mode = TRUE)
add.vertices(final_JSFnets[[3]], nv = 335, last.mode = TRUE)
add.vertices(final_JSFnets[[4]], nv = 335, last.mode = TRUE)
add.vertices(final_JSFnets[[5]], nv = 335, last.mode = TRUE)
add.vertices(final_JSFnets[[6]], nv = 335, last.mode = TRUE)
add.vertices(final_JSFnets[[7]], nv = 335, last.mode = TRUE)

#______________________________________________________________________________
#______________________________________________________________________________
# Ok let's copy the analysis section below and see if TERGMs work
#______________________________________________________________________________
#______________________________________________________________________________

library(xergm)
library(texreg)
library(network)

load(file = "final_JSF_data.RData")

# Add Committee, Contracts and Contributions as vertex attributes
# slice 1 is 109th
final_JSF_109$contracts[is.na(final_JSF_109$contracts)] <- 0
final_JSF_110$contracts[is.na(final_JSF_110$contracts)] <- 0
final_JSF_111$contracts[is.na(final_JSF_111$contracts)] <- 0
final_JSF_112$contracts[is.na(final_JSF_112$contracts)] <- 0

final_JSF_109$contrib[is.na(final_JSF_109$contrib)] <- 0
final_JSF_110$contrib[is.na(final_JSF_110$contrib)] <- 0
final_JSF_111$contrib[is.na(final_JSF_111$contrib)] <- 0
final_JSF_112$contrib[is.na(final_JSF_112$contrib)] <- 0

final_JSFnets[[1]] <- set.vertex.attribute(final_JSFnets[[1]], "contracts", 
                                           final_JSF_109$contracts)
final_JSFnets[[1]] <- set.vertex.attribute(final_JSFnets[[1]], "contrib", 
                                           final_JSF_109$contrib)
final_JSFnets[[1]] <- set.vertex.attribute(final_JSFnets[[1]], "Committee", 
                                           final_JSF_109$Committee)
# slice 2 is 110th
final_JSFnets[[2]] <- set.vertex.attribute(final_JSFnets[[2]], "contracts", 
                                           final_JSF_110$contracts)
final_JSFnets[[2]] <- set.vertex.attribute(final_JSFnets[[2]], "contrib", 
                                           final_JSF_110$contrib)
final_JSFnets[[2]] <- set.vertex.attribute(final_JSFnets[[2]], "Committee", 
                                           final_JSF_110$Committee)
# slice 3 is 110th
final_JSFnets[[3]] <- set.vertex.attribute(final_JSFnets[[3]], "contracts", 
                                           final_JSF_110$contracts)
final_JSFnets[[3]] <- set.vertex.attribute(final_JSFnets[[3]], "contrib", 
                                           final_JSF_110$contrib)
final_JSFnets[[3]] <- set.vertex.attribute(final_JSFnets[[3]], "Committee", 
                                           final_JSF_110$Committee)
# slice 4 is 111th
final_JSFnets[[4]] <- set.vertex.attribute(final_JSFnets[[4]], "contracts", 
                                           final_JSF_111$contracts)
final_JSFnets[[4]] <- set.vertex.attribute(final_JSFnets[[4]], "contrib", 
                                           final_JSF_111$contrib)
final_JSFnets[[4]] <- set.vertex.attribute(final_JSFnets[[4]], "Committee", 
                                           final_JSF_111$Committee)
# slice 5 is 111th
final_JSFnets[[5]] <- set.vertex.attribute(final_JSFnets[[5]], "contracts", 
                                           final_JSF_111$contracts)
final_JSFnets[[5]] <- set.vertex.attribute(final_JSFnets[[5]], "contrib", 
                                           final_JSF_111$contrib)
final_JSFnets[[5]] <- set.vertex.attribute(final_JSFnets[[5]], "Committee", 
                                           final_JSF_111$Committee)
# slice 6 is 112th
final_JSFnets[[6]] <- set.vertex.attribute(final_JSFnets[[6]], "contracts", 
                                           final_JSF_112$contracts)
final_JSFnets[[6]] <- set.vertex.attribute(final_JSFnets[[6]], "contrib", 
                                           final_JSF_112$contrib)
final_JSFnets[[6]] <- set.vertex.attribute(final_JSFnets[[6]], "Committee", 
                                           final_JSF_112$Committee)
# slice 7 is 112th
final_JSFnets[[7]] <- set.vertex.attribute(final_JSFnets[[7]], "contracts", 
                                           final_JSF_112$contracts)
final_JSFnets[[7]] <- set.vertex.attribute(final_JSFnets[[7]], "contrib", 
                                           final_JSF_112$contrib)
final_JSFnets[[7]] <- set.vertex.attribute(final_JSFnets[[7]], "Committee", 
                                           final_JSF_112$Committee)

final_JSFnets_0 <- list(final_JSFnets[[2]], final_JSFnets[[3]], final_JSFnets[[4]], 
                        final_JSFnets[[5]], final_JSFnets[[6]], final_JSFnets[[7]])
final_JSFnets_1 <- list(final_JSFnets[[1]], final_JSFnets[[2]], final_JSFnets[[3]], 
                        final_JSFnets[[4]], final_JSFnets[[5]], final_JSFnets[[6]])

## A basic model with only endogenous network terms:
fit1 <- btergm(final_JSFnets_0 ~ edges + edgecov(final_JSFnets_1)
                + b2star(2:3), R = 1000)

summary(fit1) 
screenreg(fit1)
gof(fit1)


## A model where we had the dummy for committee:
fit2 <- btergm(final_JSFnets_0 ~ edges +
                    edgecov(final_JSFnets_1) +
                    b2star(2:3) + 
                    nodefactor("Committee"), R = 1000)

summary(fit2)
screenreg(fit2)
gof(fit2)

# Let's add contract dollar value as an edge-level covariate
fit3 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee") +
                    edgecov(final_JSFnets_1, attrname = "dollars"), R = 1000)
summary(fit3)
screenreg(fit3)
gof(fit3)

# Let's add campaign contributions as an exogeneous covariate 
fit4 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    edgecov(final_JSFnets_1, attrname = "dollars") +
                    nodefactor("Committee") +
                    nodecov("contrib"), R = 1000)

summary(fit4)
screenreg(fit4)
gof(fit4)

# Let's add comparative advantage as an exogeneous covariate 
fit5 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee") +
                    edgecov(final_JSFnets_1, attrname = "dollars") +
                    nodecov("contracts"), R = 1000)
summary(fit5)
screenreg(fit5)
gof(fit5)

## If we change the outcome network to final_JSFnets_1 we can run a model with
## both contracts and contributions

fit6 <- btergm(final_JSFnets_1 ~ edges +
                    b2star(2:3) + 
                    edgecov(final_JSFnets_1, attrname = "dollars") +
                    nodefactor("Committee") +
                    nodecov("contracts") + 
                    nodecov("contrib"), R = 1000)
summary(fit6)
screenreg(fit6)
gof(fit6)


## A model specification with exogenous covars no lagged IV...
fit7 <- btergm(final_JSFnets ~ edges +
                               b2star(2:3) + 
                               edgecov(final_JSFnets, attrname = "dollars") +
                               nodefactor("Committee") +
                               nodecov("contracts") + 
                               nodecov("contrib"), R = 1000)

summary(fit7)

screenreg(fit7)

gof7 <- gof(fit7, target = final_JSFnets,
        formula = getformula(fit7), nsim = 100, MCMC.interval = 1000,
        MCMC.burnin = 10000, classicgof = TRUE,
        rocprgof = TRUE, checkdegeneracy = TRUE,
        dsp = TRUE, esp = TRUE, geodist = TRUE, degree = TRUE,
        idegree = FALSE, odegree = FALSE, kstar = TRUE, istar = FALSE,
        ostar = FALSE, pr.impute = "poly4", verbose = TRUE)

plot(gof7, roc = TRUE, pr = TRUE)

gof7

plot(gof7, boxplot = FALSE, roc = TRUE, pr = FALSE, 
     roc.random = TRUE, ylab = "TPR/PPV", 
     xlab = "FPR/TPR", roc.main = "ROC and PR curves")

plot(gof7, boxplot = FALSE, roc = FALSE, pr = TRUE, 
     pr.random = TRUE, rocpr.add = TRUE)



# oh shit these tergm's work so let's save them
save(fit1, fit2, fit3, fit4, fit5, fit6, fit7, gof7, file ="tergmfits.RData")
