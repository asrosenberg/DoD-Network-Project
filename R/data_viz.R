## =============================================================================
## Data Visualization for JSF Networks
## =============================================================================

# load libraries
library(ggplot2)
library(network)
library(texreg)

# load data
load("data/JSF-networks.Rdata")
load("data/relevant_cd.RData")
load("data/relevant_agencies.RData")
load("data/final-time-slices.RData")
load("data/final_JSF_data.RData")

##-----------------------------------------------------------------------------
## Spaghetti Plot for the Full F-35 Network

plot(FullNet,
     displaylabels = TRUE,
     displayisolates = FALSE,
     edge.col = "gray",
     vertex.cex = ifelse(FullNet %v% "type" == "cd", 1, 1.5),
     vertex.col = ifelse(FullNet %v% "type" != "cd", "darkred", "black"))

##-----------------------------------------------------------------------------
## Histogram of Defense Contract Values Across CDs

JSF$lndollars <- sign(JSF$dollarsobligated) * 
     log(pmax(1, abs(JSF$dollarsobligated)))

qplot(JSF$lndollars, 
      geom="histogram",
      main = "Distribution of F-35 Contract Values: FY 2005 - FY 2012", 
      xlab = "Ln of Contract Value (USD)",
      xlim=c(1,25),
      ylab = "Frequency",
      fill=I("darkred"), 
      col=I("black")
) + theme_bw()

##-----------------------------------------------------------------------------
## Histogram of Bootstrap Simulation Densities

pdf("~/Dropbox/Academic_Conferences/POLNET_2015/poster/hists-20150612.pdf", 
    width = 10,  height = 5, pointsize = 24, family = "Palatino")
ggplot(boot_df, aes(x = boots)) + 
     geom_density(fill = "#bb0000") + 
     facet_wrap(~ variable, scales = "free") + theme_bw() +
     xlab("") + ylab("") + ggtitle("Coefficient Bootstrap Distributions") +
     theme(strip.background = element_rect(fill = "white", color = "white"))
dev.off()

