library(xergm)

load(file="data/simple-time-slices.RData")


JSFnets_0 <- list(JSFnets[[2]], JSFnets[[3]], JSFnets[[4]], JSFnets[[5]], JSFnets[[6]], JSFnets[[7]])
JSFnets_1 <- list(JSFnets[[1]], JSFnets[[2]], JSFnets[[3]], JSFnets[[4]], JSFnets[[5]], JSFnets[[6]])


fit <- btergm(JSFnets ~ edges, R = 100)
summary(fit)

fit <- btergm(JSFnets_0 ~ edges + edgecov(JSFnets_1), 
              R = 100)
summary(fit)

fit <- btergm(JSFnets_0 ~ edges +
                   b2star(2:3) + 
                   edgecov(JSFnets_1), 
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
