###############################################################################
## Lockheed Martin Campaign Contributions to Congress: 2006-2012
## Data from the Sunlight Foundation
###############################################################################

require(data.table)

# setwd()

# load data
load("jsf_cd.Rdata")
load("lockheed_raw.RData")

# subset dataframe
lockheed <- subset(lockheed, select=c("cycle", "district", "amount"))

## Rename columns
names(lockheed)[1]<-"year"
names(lockheed)[2]<-"cd"

## Add NA's to cells where we have blank values
lockheed[lockheed==""]  <- NA

## Let's see how many NAs we have 
table(is.na(lockheed$cd)) # 1538 NAs
table(is.na(lockheed$amount)) # 0 NAs

## Take NA's out
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}

## listwise delete NAs for CD
lockheed <- completeFun(lockheed, "cd")

## Fix CD format to match DOD data
lockheed$cd <- gsub("*-", "", lockheed$cd)

save(lockheed, file = "lockheed_all.Rdata")

###############################################################################
## Create Time Slices for lobby contributions by campaign year

load("lockheed_all.RData")
load("jsf_cd.Rdata")

## 2006 campaign contributions from Lockheed to 115 CDs in our network
lockheed2006 <- subset(lockheed, year == 2006)  # subset by campaign2006 contrib.
lockheed2006 <- lockheed2006[, c("cd","amount")] # drop year column
lockheed2006 <- merge(jsf_cd, lockheed2006, by.x = "cd", all.x = TRUE) # merge!
unique(lockheed2006$cd) # 115 unique CDs = matches JSF rows!
lockheed2006$amount[is.na(lockheed2006$amount)] <- 0 # set NAs to 0
lockheed2006 <- data.table(lockheed2006) # for data table magic
lockheed2006 <- lockheed2006[, lapply(.SD, sum), by = cd] # sum CD contributions
lockheed2006$ln_amount <- log1p(lockheed2006$amount) # ln(ammount)

## 2008 campaign contributions from Lockheed to 115 CDs in our network
lockheed2008 <- subset(lockheed, year == 2008)  # subset by campaign2006 contrib.
lockheed2008 <- lockheed2008[, c("cd","amount")] # drop year column
lockheed2008 <- merge(jsf_cd, lockheed2008, by.x = "cd", all.x = TRUE) # merge!
unique(lockheed2008$cd) # 115 unique CDs = matches JSF rows!
lockheed2008$amount[is.na(lockheed2008$amount)] <- 0 # set NAs to 0
lockheed2008 <- data.table(lockheed2008) # for data table magic
lockheed2008 <- lockheed2008[, lapply(.SD, sum), by = cd] # sum CD contributions
lockheed2008[74, 2] <- 0 # change neg. campaign amount to 0 
lockheed2008$ln_amount <- log1p(lockheed2008$amount) # ln(ammount)

## 2010 campaign contributions from Lockheed to 115 CDs in our network
lockheed2010 <- subset(lockheed, year == 2010)  # subset by campaign2006 contrib.
lockheed2010 <- merge(jsf_cd, lockheed2010, by.x = "cd", all.x = TRUE) # merge!
unique(lockheed2010$cd) # 115 unique CDs = matches JSF rows!
lockheed2010 <- lockheed2010[, c("cd","amount")] # drop year column
lockheed2010$amount[is.na(lockheed2010$amount)] <- 0 # set NAs to 0
lockheed2010 <- data.table(lockheed2010) # for data table magic
lockheed2010 <- lockheed2006[, lapply(.SD, sum), by = cd] # sum CD contributions
lockheed2010$ln_amount <- log1p(lockheed2010$amount) # ln(ammount)

## 2012 campaign contributions from Lockheed to 115 CDs in our network
lockheed2012 <- subset(lockheed, year == 2012)  # subset by campaign2006 contrib.
lockheed2012 <- merge(jsf_cd, lockheed2012, by.x = "cd", all.x = TRUE) # merge!
unique(lockheed2012$cd) # 115 unique CDs = matches JSF rows!
lockheed2012 <- lockheed2012[, c("cd","amount")] # drop year column
lockheed2012$amount[is.na(lockheed2012$amount)] <- 0 # set NAs to 0
lockheed2012 <- data.table(lockheed2012) # for data table magic
lockheed2012 <- lockheed2012[, lapply(.SD, sum), by = cd] # sum contributions by CD
lockheed2012$ln_amount <- log1p(lockheed2012$amount) # ln(ammount)

save(lockheed2006, lockheed2008, lockheed2010, lockheed2012, 
     file = "lockheed_slices.Rdata")
