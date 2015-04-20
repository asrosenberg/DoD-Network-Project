library(xergm)
library(texreg)
library(network)

setwd("~/Dropbox/DoD/Data")
load(file = "final-time-slices.RData") 
load(file = "final_JSF_data.RData")

# Add Committee, Contracts and Contributions as vertex attributes
# slice 1 is 109th
final_JSFnets[[1]] <- set.vertex.attribute(final_JSFnets[[1]], "ln_contracts", 
                                           final_JSF_109$ln_contracts)
final_JSFnets[[1]] <- set.vertex.attribute(final_JSFnets[[1]], "ln_contrib", 
                                           final_JSF_109$ln_contrib)
final_JSFnets[[1]] <- set.vertex.attribute(final_JSFnets[[1]], "Committee", 
                                           final_JSF_109$Committee)
# slice 2 is 110th
final_JSFnets[[2]] <- set.vertex.attribute(final_JSFnets[[2]], "ln_contracts", 
                                           final_JSF_110$ln_contracts)
final_JSFnets[[2]] <- set.vertex.attribute(final_JSFnets[[2]], "ln_contrib", 
                                           final_JSF_110$ln_contrib)
final_JSFnets[[2]] <- set.vertex.attribute(final_JSFnets[[2]], "Committee", 
                                           final_JSF_110$Committee)
# slice 3 is 110th
final_JSFnets[[3]] <- set.vertex.attribute(final_JSFnets[[3]], "ln_contracts", 
                                           final_JSF_110$ln_contracts)
final_JSFnets[[3]] <- set.vertex.attribute(final_JSFnets[[3]], "ln_contrib", 
                                           final_JSF_110$ln_contrib)
final_JSFnets[[3]] <- set.vertex.attribute(final_JSFnets[[3]], "Committee", 
                                           final_JSF_110$Committee)
# slice 4 is 111th
final_JSFnets[[4]] <- set.vertex.attribute(final_JSFnets[[4]], "ln_contracts", 
                                           final_JSF_111$ln_contracts)
final_JSFnets[[4]] <- set.vertex.attribute(final_JSFnets[[4]], "ln_contrib", 
                                           final_JSF_111$ln_contrib)
final_JSFnets[[4]] <- set.vertex.attribute(final_JSFnets[[4]], "Committee", 
                                           final_JSF_111$Committee)
# slice 5 is 111th
final_JSFnets[[5]] <- set.vertex.attribute(final_JSFnets[[5]], "ln_contracts", 
                                           final_JSF_111$ln_contracts)
final_JSFnets[[5]] <- set.vertex.attribute(final_JSFnets[[5]], "ln_contrib", 
                                           final_JSF_111$ln_contrib)
final_JSFnets[[5]] <- set.vertex.attribute(final_JSFnets[[5]], "Committee", 
                                           final_JSF_111$Committee)
# slice 6 is 112th
final_JSFnets[[6]] <- set.vertex.attribute(final_JSFnets[[6]], "ln_contracts", 
                                           final_JSF_112$ln_contracts)
final_JSFnets[[6]] <- set.vertex.attribute(final_JSFnets[[6]], "ln_contrib", 
                                           final_JSF_112$ln_contrib)
final_JSFnets[[6]] <- set.vertex.attribute(final_JSFnets[[6]], "Committee", 
                                           final_JSF_112$Committee)
# slice 7 is 112th
final_JSFnets[[7]] <- set.vertex.attribute(final_JSFnets[[7]], "ln_contracts", 
                                           final_JSF_112$ln_contracts)
final_JSFnets[[7]] <- set.vertex.attribute(final_JSFnets[[7]], "ln_contrib", 
                                           final_JSF_112$ln_contrib)
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
     tmp <- (final_JSFnets[[i]] %v% "ln_contrib")
     tmp[88:94] <- NA
     final_JSFnets[[i]] %v% "ln_contrib" <- tmp
}

for(i in 1:length(final_JSFnets)){
     tmp <- (final_JSFnets[[i]] %v% "ln_contracts")
     tmp[88:94] <- NA
     final_JSFnets[[i]] %v% "ln_contracts" <- tmp
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
fit1 <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(final_JSFnets_1), 
              R = 1000)
summary(fit1) 

fit2 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    edgecov(final_JSFnets_1) + nodefactor("Committee"), 
               R = 1000)
summary(fit2)

fit3 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    edgecov(final_JSFnets_1) + nodefactor("Committee")
               + edgecov(final_JSFnets_1, attrname = "dollars"), 
               R = 5000)
summary(fit3, level = 0.90)
screenreg(fit3)

fit4 <- btergm(final_JSFnets_0 ~ edges +
                    b2star(2:3) + 
                    edgecov(final_JSFnets_1) + nodefactor("Committee")
               + edgecov(final_JSFnets_1, attrname = "dollars")
               + nodematch("ln_contrib"), 
               R = 1000)
summary(fit4)

fit <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(final_JSFnets_1) + nodecov("Contributions") + 
                   nodecov("Contracts") + nodecov("Committee"), 
              R = 10000)
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
