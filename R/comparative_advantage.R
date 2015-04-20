###############################################################################
## Pre-JSF Comparative Advantage Variable
###############################################################################

#setwd

load("dod_all.RData")

dod_all <- subset(dod, select = c("congressionaldistrict",
                                  "dollarsobligated",
                                  "mod_parent",
                                  "principalnaicscode",
                                  "systemequipmentcode"))

## Rename columns
names(dod_all)[1]<-"cd"
names(dod_all)[2]<-"dollars"
names(dod_all)[3]<-"contractor"
names(dod_all)[4]<-"naics"
names(dod_all)[5]<-"platform"

## make a new congressional district field
## copy in old congresionaldistrict
## change values "MS00" and "TX00"
dod_all$cd[which(dod_all$cd == "MS00")] <- "MS04"
dod_all$cd[which(dod_all$cd == "TX00")] <- "TX12"
dod_all$cd[which(dod_all$cd == "MT00")] <- "MT01"


## Add NA's to cells where we have blank values
dod_all[dod_all==""]  <- NA

## Take NA's out
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}

dod_all <- completeFun(dod_all)

save(dod_all, file = "dod_2004.Rdata")


###############################################################################
require(data.table)

load("dod_2004.RData")
load("jsf_cd.RData")
load("JSF_naics.RData")

## Remove JSF contracts to get a measure of Pre-JSF comparative advantage
unique(dod_all$platform) ## 314 unique defense platforms

dod_all <- subset(dod_all, dod_all$platform != "198: JSF (F-35) ") # remove JSF programs

dod_all <- subset(dod_all, dod_all$contractor != "LOCKHEED MARTIN CORPORATION")

dod_all <- merge(jsf_cd, dod_all, by.x = "cd", all.x = TRUE) # merge!

unique(dod_all$cd) # 115 unique CDs = matches JSF rows!

dod_all$dollars[is.na(dod_all$dollars)] <- 0 # set NAs to 0

dod_all <- merge(JSF_naics, dod_all, by.x = "naics", all.x = TRUE)



######

dod_all <- data.table(dod_all) # for data table magic

dod_all <- dod_all[, .(sum=sum(dollars)), by=.(cd)] # sum CD contributions

names(dod_all)[2]<-"dollars"

#dod_all$ln_dollars <- log1p(dod_all$dollars) # ln(dollars)

#dod_all <- dod_all[-116,]

dod_all[115, 2] <- 0
     
#dod_all$ln_dollars <- log1p(dod_all$dollars) # ln(dollars)

save(dod_all, file = "comparative_advantage.Rdata")


