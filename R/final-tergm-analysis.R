library(xergm)
library(texreg)

load(file="final-time-slices.RData")

# Add Committee, Contracts and Contributions as vertex attributes

for (i in 1:length(final_JSFnets)){
     final_JSFnets[[i]] <- set.vertex.attribute(final_JSFnets[[i]], "Committee", final_JSF$Committee) # add as vertex attribute     
     final_JSFnets[[i]] <- set.vertex.attribute(final_JSFnets[[i]], "Contracts", final_JSF$ln_contracts) # add as vertex attribute     
     final_JSFnets[[i]] <- set.vertex.attribute(final_JSFnets[[i]], "Contributions", final_JSF$ln_contrib) # add as vertex attribute     
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

fit <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(final_JSFnets_1), 
              R = 1000)
summary(fit)

fit <- btergm(JSFnets_0 ~ edges +
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(JSFnets_1), 
              R = 1000)
summary(fit)


fit <- btergm(JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(JSFnets_1), 
              R = 1000)
summary(fit)


# add dollars (or don't)
fit <- btergm(JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(JSFnets_1) +
                   edgecov(JSFnets_1, attrname = "dollars"), 
              R = 1000)
summary(fit) 

# TERGM with each new vertex attribute
fit <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   gwb1degree(.5, fixed = TRUE) +
                   edgecov(final_JSFnets_1) + nodecov("Committee"), 
              R = 1000)
summary(fit) 

fit <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(final_JSFnets_1) + nodecov("Contributions") + 
                   nodecov("Contracts") + nodecov("Committee"), 
              R = 10000)
summary(fit)

fit <- btergm(final_JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(final_JSFnets_1) + nodecov("Contributions") + 
                   nodecov("Contracts"), 
              R = 1000)
summary(fit, level = 0.95)
gof(fit)

screenreg(fit)
