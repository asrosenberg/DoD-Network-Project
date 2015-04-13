###############################################################################
## Pre-JSF Comparative Advantage Variable
###############################################################################

#setwd("~/Desktop/JSF/comparative_advantage")

load("dod_all.RData")

dod <- subset(dod_all, select = c("congressionaldistrict",
                              "dollarsobligated",
                              "mod_parent",
                              "principalnaicscode",
                              "systemequipmentcode"))

## Rename columns
names(dod)[1]<-"cd"
names(dod)[2]<-"dollars"
names(dod)[3]<-"contractor"
names(dod)[4]<-"platform"


##Add NA's to cells where we have blank values
dod[dod==""]  <- NA

## Take NA's out
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}

dod <- completeFun(dod)

save(dod, file = "dod_2004.Rdata")

jsf_cd <- read.csv("~/Desktop/JSF/cd/cd.csv")
save(jsf_cd, file = "jsf_cd.Rdata")

###############################################################################
require(data.table)

load("~/Desktop/JSF/comparative_advantage/dod_2004.RData")
load("~/Desktop/JSF/cd/jsf_cd.RData")

## Remove JSF contracts to get a measure of Pre-JSF comparative advantage
unique(dod$platform) ## 314 unique defense platforms

dod1 <- subset(dod, dod$platform != "198: JSF (F-35) ") # remove JSF programs

dod1 <- subset(dod, dod$contractor != "LOCKHEED MARTIN CORPORATION")

dod1 <- dod[, c("cd","dollars")] # drop year column

dod1 <- merge(jsf_cd, dod1, by.x = "cd", all.x = TRUE) # merge!

unique(dod1$cd) # 115 unique CDs = matches JSF rows!

dod1$dollars[is.na(dod1$dollars)] <- 0 # set NAs to 0

dod1 <- data.table(dod1) # for data table magic

dod1 <- dod1[, lapply(.SD, sum), by = cd] # sum CD contributions

dod1$ln_dollars <- log1p(dod1$dollars) # ln(ammount)

save(dod1, file = "comparative_advantage.Rdata")


