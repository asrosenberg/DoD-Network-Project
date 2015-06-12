#______________________________________________________________________________
#______________________________________________________________________________
# Let's add a vector of 0s for each of our time slices
#______________________________________________________________________________
#______________________________________________________________________________
library(network)
library(xergm)
library(texreg)
library(network)
library(plyr)


#setwd("~/Dropbox/DoD/data")
load("data/JSF-networks.Rdata")
load("data/relevant_cd.RData")
load("data/relevant_agencies.RData")
load("data/final-time-slices.RData")
load(file = "data/final_JSF_data.RData")

# We need to see if the network package can auto add nodes to our bipartite net
# add.vertices(final_JSFnets[[1]], nv = 16, last.mode = TRUE)
# add.vertices(final_JSFnets[[2]], nv = 16, last.mode = TRUE)
# add.vertices(final_JSFnets[[3]], nv = 16, last.mode = TRUE)
# add.vertices(final_JSFnets[[4]], nv = 16, last.mode = TRUE)
# add.vertices(final_JSFnets[[5]], nv = 16, last.mode = TRUE)
# add.vertices(final_JSFnets[[6]], nv = 16, last.mode = TRUE)
# add.vertices(final_JSFnets[[7]], nv = 16, last.mode = TRUE)
# 
# add.vertices(final_JSFnets[[1]], nv = 335, last.mode = FALSE)
# add.vertices(final_JSFnets[[2]], nv = 335, last.mode = FALSE)
# add.vertices(final_JSFnets[[3]], nv = 335, last.mode = FALSE)
# add.vertices(final_JSFnets[[4]], nv = 335, last.mode = FALSE)
# add.vertices(final_JSFnets[[5]], nv = 335, last.mode = FALSE)
# add.vertices(final_JSFnets[[6]], nv = 335, last.mode = FALSE)
# add.vertices(final_JSFnets[[7]], nv = 335, last.mode = FALSE)

#______________________________________________________________________________
#______________________________________________________________________________
# Ok let's copy the analysis section below and see if TERGMs work
#______________________________________________________________________________
#______________________________________________________________________________


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


add_nodecovs <- function(net_in, df) {
     net_out <- net_in
     vertex_names <- do.call(c, lapply(net_in$val, function(i) i$vertex.name))
     
     net_out <- set.vertex.attribute(net_out, "contrib", 0)
     net_out <- set.vertex.attribute(net_out, "contracts", 0)
     net_out <- set.vertex.attribute(net_out, "Committee", 0)
     
     to_change <- ddply(df, .(cd), summarise, 
                        contrib = (mean(contrib)) / 1e3,
                        contracts = (mean(contracts)) / 1e9,
                        Committee = mean(Committee))
     for (i in 1:nrow(to_change)) {
          cd_to_change <- to_change$cd[i]
          ok <- which(vertex_names == cd_to_change)
          net_out$val[[ok]]$contrib <- to_change$contrib[i]
          net_out$val[[ok]]$contracts <- to_change$contracts[i]
          net_out$val[[ok]]$Committee <- to_change$Committee[i]
     }
     
     net_out
}
final_JSFnets[[1]] <- add_nodecovs(final_JSFnets[[1]], final_JSF_109)
final_JSFnets[[2]] <- add_nodecovs(final_JSFnets[[2]], final_JSF_110)
final_JSFnets[[3]] <- add_nodecovs(final_JSFnets[[3]], final_JSF_110)
final_JSFnets[[4]] <- add_nodecovs(final_JSFnets[[4]], final_JSF_111)
final_JSFnets[[5]] <- add_nodecovs(final_JSFnets[[5]], final_JSF_111)
final_JSFnets[[6]] <- add_nodecovs(final_JSFnets[[6]], final_JSF_112)
final_JSFnets[[7]] <- add_nodecovs(final_JSFnets[[7]], final_JSF_112)


# Lag the network to account for comparative advantage:
final_JSFnets_0 <- list(final_JSFnets[[2]], final_JSFnets[[3]], final_JSFnets[[4]], 
                        final_JSFnets[[5]], final_JSFnets[[6]], final_JSFnets[[7]])
final_JSFnets_1 <- list(final_JSFnets[[1]], final_JSFnets[[2]], final_JSFnets[[3]], 
                        final_JSFnets[[4]], final_JSFnets[[5]], final_JSFnets[[6]])

## Let's plot out the slices to see what they look like with all the isolates:

par(mfrow = c(4, 2), mar = c(0, 0, 1, 0)) #mar = margins command 

plot(final_JSFnets[[1]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2005-2006")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

plot(final_JSFnets[[2]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2006-2007")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

plot(final_JSFnets[[3]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2007-2008")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

plot(final_JSFnets[[4]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2008-2009")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

plot(final_JSFnets[[5]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2009-2010")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

plot(final_JSFnets[[6]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2010-2011")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

plot(final_JSFnets[[7]],
     displaylabels=FALSE,
     pad=0,
     edge.col="gray",
     vertex.border=FALSE,
     vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
     vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
     main = "FY 2011-2012")
     #legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
     #pch = 19)

#______________________________________________________________________________
#______________________________________________________________________________
# Time to analyze the TERGM model specifications
#______________________________________________________________________________
#______________________________________________________________________________

## a naive model specification
fit0 <- btergm(final_JSFnets ~ edges + b2star(2:3), R = 1000)
screenreg(fit0)
gof(fit0)

## A basic model with only endogenous network terms:
fit1 <- btergm(final_JSFnets ~ edges + 
                     b2star(2:3) +
                     edgecov(final_JSFnets, attrname = "dollars"), R = 1000)

summary(fit1) 
screenreg(fit1)
gof(fit1)


## A model where we had the dummy for committee:
fit2 <- btergm(final_JSFnets ~ edges +
                    edgecov(final_JSFnets, attrname = "dollars") +
                    b2star(2:3) + 
                    nodefactor("Committee"), R = 1000)

summary(fit2)
screenreg(fit2)
gof(fit2)


# Let's add campaign contributions as an exogeneous covariate 
fit3 <- btergm(final_JSFnets ~ edges +
                    b2star(2:3) + 
                    edgecov(final_JSFnets, attrname = "dollars") +
                    nodefactor("Committee") +
                    nodecov("contrib"), R = 1000)

summary(fit3)
screenreg(list(fit3)
gof(fit3)

# Let's add comparative advantage as an exogeneous covariate 
fit4 <- btergm(final_JSFnets ~ edges +
                    b2star(2:3) + 
                    nodefactor("Committee") +
                    edgecov(final_JSFnets, attrname = "dollars") +
                    nodecov("contracts"), R = 1000)
summary(fit4)
screenreg(fit4)
gof(fit4)

## A model specification with the works... 
fit5 <- btergm(final_JSFnets ~ edges +
                               b2star(2:3) + 
                               edgecov(final_JSFnets, attrname = "dollars") +
                               nodefactor("Committee") +
                               nodecov("contracts") + 
                               nodecov("contrib"), R = 1000)

summary(fit5)
screenreg(fit5)

gof5 <- gof(fit5)
plot(gof5, roc = TRUE, pr = TRUE)

gof5

plot(gof5, boxplot = FALSE, roc = TRUE, pr = FALSE, 
     roc.random = TRUE, ylab = "TPR/PPV", 
     xlab = "FPR/TPR", roc.main = "ROC and PR curves")

plot(gof5, boxplot = FALSE, roc = FALSE, pr = TRUE, 
     pr.random = TRUE, rocpr.add = TRUE)


## coefficient plot for fit 5
plotreg(fit5, 
        lwd.inner = 1,
        custom.model.names = "TERGM Coefficient Plot", 
        omit.coef = "Edges",
        custom.coef.names =
             c("Edges",
               "2-stars (Mode 2)", 
               "3-stars (Mode 2)", 
               "Contract Value (Edge Cov.)", 
               "Committee (Node Factor)", 
               "Comparative Advantage (Node Cov.)",
               "Campaign Contributions (Node Cov.)")
)

## another coefficient plot 
install.packages("coefplot")
library("coefplot")

coefplot(fit5)

## Fit 6
fit6 <- btergm(final_JSFnets ~ nodecov("contracts") + 
                    nodefactor("Committee") + 
                    nodecov("contrib") +
                    b2star(2:3) + 
                    edges, R = 10000)
screenreg(fit6)
## coefficient plot for fit 5
plotreg(fit6, 
        lwd.inner = 1,
        custom.model.names = "TERGM Coefficient Plot", 
        omit.coef = "Edges",
        custom.coef.names =
             c("Contract Value (Edge Cov.)", 
               "Committee (Node Factor)", 
               "Campaign Contributions (Node Cov.)",
               "2-stars (Mode 2)", 
               "3-stars (Mode 2)", 
               "Edges"
               )
)

coefplot(fit6)
vars <- c("Prev. District Defense Contracts\nin $Billions", 
          "Committee Member\n0-1", 
          "Campaign Contributions\nin $10k",
          "2-stars\nDistrict-level", 
          "3-stars\nDistrict-level", 
          "Edges")
results_df <- data.frame(
     Estimate = coef(fit6),
     Q025 = apply(fit6@bootsamp, 2, quantile, .025),
     Q975 = apply(fit6@bootsamp, 2, quantile, .975),
     Variable = factor(vars, level = rev(vars)),
     color = factor(rep(1:2, each = 3))
     )
library(ggplot2)

pdf("~/Dropbox/Academic_Conferences/POLNET_2015/coefplot-20150612.pdf", 
    width = 10,  height = 5, pointsize = 24, family = "Palatino")
ggplot(subset(results_df, Variable != "Edges"),
       aes(x = Estimate, xmin = Q025, xmax = Q975, y = Variable, color = color, fill = color)) +
     geom_vline(xintercept = 0, linetype = 3, color = "gray") +
     geom_errorbarh(height = 0, size = 1, lineend = "round") +
     geom_point(size = 4) +
     theme_bw() +
     ylab("") + xlab("") +
     ggtitle("TERGM coefficients") +
     scale_color_manual(name = "color", values = c("#bb0000", "#666666")) +
     theme(legend.position = "none")
dev.off()

boot_df <- data.frame(
     boots <- as.vector(fit6@bootsamp),
     variable = factor(rep(vars, each = 10000), levels = (vars))
)

pdf("~/Dropbox/Academic_Conferences/POLNET_2015/hists-20150612.pdf", 
    width = 10,  height = 5, pointsize = 24, family = "Palatino")
ggplot(boot_df, aes(x = boots)) + 
     geom_density(fill = "#bb0000") + 
     facet_wrap(~ variable, scales = "free") + theme_bw() +
     xlab("") + ylab("") + ggtitle("Coefficient Bootstrap Distributions") +
     theme(strip.background = element_rect(fill = "white", color = "white"))
dev.off()


N <- 1000
Dta <- data.frame(x=sample(c(0,1), N, replace=TRUE),
                  z=rnorm(N, sd=2))
Dta$y  <- 0.5 + 0.25*Dta$x - 3.0*Dta$x*Dta$z + rnorm(N)
Dta$y2 <- 0.5 + 0.25*Dta$x - 3.5*Dta$x*Dta$z + rnorm(N, sd=2)

summary(mod1 <- lm(y  ~ x*z, data=Dta))
summary(mod2 <- lm(y2 ~ x*z, data=Dta))

newdata <- data.frame(x=c(0, 1), z=c(mean(Dta$z), mean(Dta$z)))

pred1 <- predict(mod1, newdata=newdata, se.fit=TRUE)
pred2 <- predict(mod2, newdata=newdata, se.fit=TRUE)

low1  <- qnorm(0.05, pred1$fit, sd=pred1$se.fit)
high1 <- qnorm(0.95, pred1$fit, sd=pred1$se.fit)
low2  <- qnorm(0.05, pred2$fit, sd=pred2$se.fit)
high2 <- qnorm(0.95, pred2$fit, sd=pred2$se.fit)

Eff <- cbind(pred1$fit, pred2$fit)
colnames(Eff) <- c("Model 1", "Model 2")
rownames(Eff) <- c("x = 0", "x = 1")

pdf(file="coefplot.pdf", width=8, height=4)
dotchart(Eff, xlim=c(0, 1), pch=19, col=c("red", "blue"))
lines(c(low1[1], high1[1]), y=c(5,5), col="red")
lines(c(low1[2], high1[2]), y=c(6,6), col="blue")
lines(c(low2[1], high2[1]), y=c(1,1), col="red")
lines(c(low2[2], high2[2]), y=c(2,2), col="blue")
dev.off()


## Fit 7
fit7 <- btergm(final_JSFnets ~ nodecov("contracts") + 
                    nodecov("contrib") +
                    b2star(2:3) + 
                    edges, R = 1000)

screenreg(fit7)



# oh shit these tergm's work so let's save them
save(fit0, fit1, fit2, fit3, fit4, fit5, file ="tergmfits.RData")

#load("tergmfits.RData")


# look at nodecovs' distribution

df_list <- list(final_JSF_109, final_JSF_110, 
                final_JSF_111, final_JSF_112)

x <- do.call(rbind, lapply(df_list, function(df) ddply(df, 
            .(cd), summarise, 
            contrib = (mean(contrib)),
            contracts = (mean(contracts)) ,
            Committee = mean(Committee))))

