library(xergm)
library(texreg)
library(network)

setwd("~/Dropbox/DoD/Data")
load(file = "final-time-slices.RData") 
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



#____________________________________________________________________________________
#____________________________________________________________________________________
# Ok we need to make sure only mode 2 nodes have attributes
#____________________________________________________________________________________
#____________________________________________________________________________________
slice_one_names <- as.vector(final_JSFnets[[1]] %v% "vertex.names")
slice_two_names <- as.vector(final_JSFnets[[2]] %v% "vertex.names")
slice_three_names <- as.vector(final_JSFnets[[3]] %v% "vertex.names")
slice_four_names <- as.vector(final_JSFnets[[4]] %v% "vertex.names")

for(i in 1:length(final_JSFnets)){
     tmp <- (final_JSFnets[[i]] %v% "contrib")
     tmp[88:94] <- NA
     final_JSFnets[[i]] %v% "contrib" <- tmp
}

for(i in 1:length(final_JSFnets)){
     tmp <- (final_JSFnets[[i]] %v% "contracts")
     tmp[88:94] <- NA
     final_JSFnets[[i]] %v% "contracts" <- tmp
}

for(i in 1:length(final_JSFnets)){
     tmp <- (final_JSFnets[[i]] %v% "Committee")
     tmp[88:94] <- NA
     final_JSFnets[[i]] %v% "Committee" <- tmp
}

final_JSFnets_0 <- list(final_JSFnets[[2]], final_JSFnets[[3]], final_JSFnets[[4]], 
                        final_JSFnets[[5]], final_JSFnets[[6]], final_JSFnets[[7]])
final_JSFnets_1 <- list(final_JSFnets[[1]], final_JSFnets[[2]], final_JSFnets[[3]], 
                        final_JSFnets[[4]], final_JSFnets[[5]], final_JSFnets[[6]])


save(final_JSFnets, final_JSFnets_0, final_JSFnets_1, file = "analysis.RData")


#fit <- btergm(final_JSFnets ~ edges, R = 100)
#summary(fit)

fit <- btergm(final_JSFnets_0 ~ edges + edgecov(final_JSFnets_1), 
              R = 100)
summary(fit)

fit1 <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(final_JSFnets_1), 
              R = 1000)
summary(fit1)

model <- btergm(final_JSFnets_0 ~ edges +
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(final_JSFnets_1), 
              R = 1000)
summary(model)


fit3 <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(final_JSFnets_1), 
              R = 1000)
summary(fit3)
gof3 <- gof(fit3)


# add dollars (or don't)
fit <- btergm(JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(JSFnets_1) +
                   edgecov(JSFnets_1, attrname = "dollars"), 
              R = 1000)
summary(fit) 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Analysis from here
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# TERGM with each new vertex attribute
library(xergm)
library(texreg)
library(network)
setwd("~/Dropbox/DoD/data")
load("analysis.RData")

fit1 <- btergm(final_JSFnets_0 ~ edges +
                    + edgecov(final_JSFnets_1)
                   b2star(2:3), 
              R = 1000)

summary(fit1) 
screenreg(fit1)
gof(fit1)

fit2 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee"), 
               R = 1000)

summary(fit2)
screenreg(fit2)
gof(fit2)

fit3 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee")
               + edgecov(final_JSFnets_1, attrname = "dollars"), 
               R = 1000)

summary(fit3, level = 0.90)
screenreg(fit3)
<<<<<<< HEAD
gof3 <- gof(fit3)
=======
gof_3 <- gof(fit3, nsim = 25) # Want BIG p-values here

pdf("gof_3.pdf")
plot(gof_3)
dev.off()
>>>>>>> 72a675111ea94a0d6ea1b847550d3e79c89a8950

fit4 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee")
               + edgecov(final_JSFnets_1, attrname = "dollars")
               + nodecov("contrib"), 
               R = 1000)
summary(fit4)
screenreg(fit4)
plotreg(list(fit3, fit4))

fit5 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee")
               + edgecov(final_JSFnets_1, attrname = "dollars")
               + nodecov("contracts"), 
               R = 1000)
summary(fit5)
screenreg(fit4)


fit <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   nodecov("contrib") + 
                   nodecov("contracts") + nodecov("Committee"), 
              R = 1000)
summary(fit)

final_JSFnets_0[[1]] %v% "Contributions"
final_JSFnets_0[[1]] %v% "Contributions" <- factor(final_JSFnets_0[[1]] %v% "Contributions")
fit <- btergm(final_JSFnets_0 ~ edges + b2star(2:3) + 
                   edgecov(final_JSFnets_1) + 
                   nodefactor(as.factor(as.character(("Contributions"))) + 
               nodefactor(as.factor(as.character("Contracts"))) + 
                    nodefactor("Committee"), R = 1000))
summary(fit, level = 0.95)
gof(fit)

screenreg(fit)
