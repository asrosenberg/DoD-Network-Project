###########################################################################################################
###########################################################################################################
## What's the universe of possible cases for the JSF network?
###########################################################################################################

## setwd()

## load vector of CDs from the observed JSF network
load("jsf_cd.RData") # There are 115 connected CDs in our observed network

## load vector of all CDs in the US from Stuart & Woon 
load("all_cd.RData") # There are 441 CDs = 435 + DC + non-voting territories

## Puzzle: How do we determine the universe of all possible DoD agencies and CDs?

## Let's load the complete DoD contract data from FY 2004
load("dod_all.RData")

## First, we'll subset the dataframe to make it more manageable
dod_2004 <- subset(dod, select=c("fiscal_year",
                                 "contractingofficeagencyid", 
                                 "congressionaldistrict", 
                                 "mod_parent",
                                 "dollarsobligated",
                                 "principalnaicscode",
                                 "systemequipmentcode")) 

## We no longer need the raw DOD 2004 data
rm(dod)

## Let's rename the columns to make it more manageable to call variables"
names(dod_2004)[1]<-"year"
names(dod_2004)[2]<-"agency"
names(dod_2004)[3]<-"cd"
names(dod_2004)[4]<-"contractor"
names(dod_2004)[5]<-"contract_value"
names(dod_2004)[6]<-"naics"
names(dod_2004)[7]<-"platform"

## Let's assign and remove NAs for the CDs and agencies

## Add NA's to cells where we have blank values
dod_2004[dod_2004 == ""] <- NA

## Now remove the NAs
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}

dod_2004 <- completeFun(dod_2004)

## Let's fix the problem of unassigned CDs in the remaining data
## For states with a single, at large, CD, we will code 01
dod_2004$cd[which(dod_2004$cd == "MT00")] <- "MT01"
dod_2004$cd[which(dod_2004$cd == "DC00")] <- "DC01"
dod_2004$cd[which(dod_2004$cd == "AK00")] <- "AK01"
dod_2004$cd[which(dod_2004$cd == "WY00")] <- "WY01"
dod_2004$cd[which(dod_2004$cd == "DE00")] <- "DE01"
dod_2004$cd[which(dod_2004$cd == "ND00")] <- "ND01"
dod_2004$cd[which(dod_2004$cd == "SD00")] <- "SD01"

## Let's also assign all those Northrop and Lockheed contracts to the correct CD
dod_2004$cd[which(dod_2004$cd == "MS00")] <- "MS04" ## Northrop Grumman HQ
dod_2004$cd[which(dod_2004$cd == "TX00")] <- "TX12" ## Lockheed HQ

## Before moving on, let's save the clean version of DOD_2004
save(dod_2004, file = "dod_2004_clean.RData")

################################################################################
## ANALYSIS TO DETERMINE RELEVANT AGENCIES AND CDs
################################################################################
load("dod_2004_clean.RData")
load("jsf_cd.RData")
load("all_cd.RData")

unique(dod_2004$agency) ## 23 relevant federal agencies in DOD_2004

## Let's use these 23 agencies as our universe of possible agencies for the
## JSF program -- I believe we have 7 agencies in our observed JSF network

## Let's save a vector of them for later use
relevant_agencies <- unique(as.vector(dod_2004$agency)) # create a vector
relevant_agencies <- as.data.frame(relevant_agencies) # make into data frame
write.table(relevant_agencies, "relevant_agencies.csv", sep="\t") # CSV
save(relevant_agencies, file = "relevant_agencies.RData") # RData

## Now let's sort out that pesky CD problem

unique(dod_2004$cd) ## 731 CDs -- WTF?! NO!!!!

## To determine the  CDs that are politically relevant for our study, let's 
## subset the dod_2004 by the NAICs code explicitly used for military aircraft

## It looks like there are three:
## "336411: AIRCRAFT MANUFACTURING"
## "336412: AIRCRAFT ENGINE AND ENGINE PARTS MANUFACTURING"
## "336413: OTHER AIRCRAFT PARTS AND AUXILIARY EQUIPMENT MANUFACTURING"

## Let's subset the dod_2004 data by these three NAICs codes and see what we get 
dod_2004 <- dod_2004[which(dod_2004$naics == "336411: AIRCRAFT MANUFACTURING"
                           | dod_2004$naics == "336412: AIRCRAFT ENGINE AND ENGINE PARTS MANUFACTURING"
                           | dod_2004$naics == "336413: OTHER AIRCRAFT PARTS AND AUXILIARY EQUIPMENT MANUFACTURING"), ]

## This still leaves us with 36,253 DOD contract for FY2004 -- plenty! 

## How many unique agenices and CDs are there now?
unique(dod_2004$cd) ## 432 CDs -- much better!

## It looks like we still have "irrelevant" districts (e.g. CA 00, Guam, etc.)
## Let's merge dod_2004 by the complete list of CDs from Stuart & Woon
## This will remove US territories but keep the 436 potential CDs we care about

dod_2004 <- merge(all_cd, dod_2004, by.x = "cd")

## Let's remove the few remaining irrelvant territories
dod_2004 <- subset(dod_2004, dod_2004$cd != "PR00") ## Sorry Puerto Rico!
dod_2004 <- subset(dod_2004, dod_2004$cd != "GU00") ## Sorry Guam!
dod_2004 <- subset(dod_2004, dod_2004$cd != "AS00") ## Sorry American Somoa!

dod_2004 <- subset(dod_2004, dod_2004$cd != "CA00")
dod_2004 <- subset(dod_2004, dod_2004$cd != "IA00")
dod_2004 <- subset(dod_2004, dod_2004$cd != "NJ00")
dod_2004 <- subset(dod_2004, dod_2004$cd != "CT00")
dod_2004 <- subset(dod_2004, dod_2004$cd != "GA00")
dod_2004 <- subset(dod_2004, dod_2004$cd != "VT00")
dod_2004 <- subset(dod_2004, dod_2004$cd != "NJ26") ## Jersey doesnt have 26 CDs!

dod_2004[dod_2004 == "NH30"] <- "NH03" ## Looks like NH03 was mis-coded
dod_2004[dod_2004 == "DC10"] <- "DC01" ## Looks like DC01 was also mis-coded

## This leaves us with 422 relevant CDs for our universe for the JSF program
unique(dod_2004$cd) 

## Save the relevant CDs as a vector and dataframe for later use 
relevant_cd <- unique(as.vector(dod_2004$cd)) # as a vector
relevant_cd <- as.data.frame(relevant_cd) # as a data frame
names(relevant_cd)[1]<-"cd" # fix the column name
write.table(relevant_cd, "relevant_cd.csv", sep="\t") # write a CSV
save(relevant_cd, file = "relevant_cd.RData") # write RData