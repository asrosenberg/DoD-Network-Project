# 109-112 Congresses
# Get house assignments data and subset by Appropriations, Armed Services

setwd("~/Dropbox/DoD/data")

house_assignments <- read.csv("stewart_woon.csv", header = TRUE)
colnames(house_assignments) <- NA
cols <- as.matrix(house_assignments[1,])
cols[,19] <- "state_name"
cols[,16] <- "committee"
colnames(house_assignments) <- cols
house_assignments <- house_assignments[2:9255,]


#rownames(house_assignments) <- NULL

house_assignments$Congress <- as.numeric(as.character(house_assignments$Congress))

house_assignments <- subset(house_assignments, Congress > 108)

#house_assignments <- subset(house_assignments, committee == "Appropriations" | 
#                                 committee == "Armed Services")

house_assignments$cd <- paste(house_assignments$state_name, 
                                      house_assignments$CD, sep = '')



## Load in JSF data to merge in house_assignments data
## Load data
JSF <- read.csv("JSF.csv.bz2", stringsAsFactors = FALSE)

## Just keep a few columns
JSF <- subset(JSF, select=c("agencyid",
                            "baseandalloptionsvalue",
                            "congressionaldistrict",
                            "contractingofficeagencyid",
                            "contractingofficeid",
                            "dollarsobligated",
                            "dunsnumber",
                            "fiscal_year",
                            "fundingrequestingofficeid",
                            "mod_agency",
                            "mod_parent",
                            "modnumber",
                            "parentdunsnumber",
                            "piid",
                            "idvpiid",
                            "placeofperformancecongressionaldistrict",
                            "principalnaicscode",
                            "systemequipmentcode",
                            "unique_transaction_id",
                            "zipcode",
                            "last_modified_date",
                            "signeddate",
                            "effectivedate",
                            "currentcompletiondate",
                            "ultimatecompletiondate",
                            "lastdatetoorder"))

JSF <- JSF[order(JSF$idvpiid, JSF$piid), ]

## Add NA's to cells where we have blank values
JSF[JSF==""]  <- NA

# Make into R Date format
JSF$effectivedate          <- as.Date(JSF$effectivedate, "%m/%d/%Y")
JSF$last_modified_date     <- as.Date(JSF$last_modified_date, "%m/%d/%Y")
JSF$signeddate             <- as.Date(JSF$signeddate, "%m/%d/%Y")
JSF$currentcompletiondate  <- as.Date(JSF$currentcompletiondate, "%m/%d/%Y")
JSF$ultimatecompletiondate <- as.Date(JSF$ultimatecompletiondate, "%m/%d/%Y")
JSF$lastdatetoorder        <- as.Date(JSF$lastdatetoorder, "%m/%d/%Y")

## make a new congressional district field
## copy in old congresionaldistrict
## change values "MS00" and "TX00" and "MT00"
JSF$cd <- JSF$congressionaldistrict
JSF$cd[which(JSF$congressionaldistrict == "MS00")] <- "MS04"
JSF$cd[which(JSF$congressionaldistrict == "TX00")] <- "TX12"
JSF$cd[which(JSF$congressionaldistrict == "MT00")] <- "MT01"

## Let's see how many NAs we have for CD
table(is.na(JSF$cd))  # missingness here from multinational firms
table(is.na(JSF$contractingofficeagencyid))

## Take NA's out
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}

JSF <- completeFun(JSF, "cd")


JSF$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", JSF$congressionaldistrict, perl=TRUE)
## Create temporal slices by Congress. JWM: Notice I added the 108th and 113th
## congresses because many contracts had dates earlier than the 109th and there
## was one in the 113th.
library(data.table)

start <- as.Date(c(
     "01/03/2005", # 109
     "01/03/2007",  # 110
     "01/03/2009",  # 111
     "01/03/2011",  # 112
     "01/03/2013"), 
     format = "%m/%d/%Y")  # 113

end   <- as.Date(c(
     "01/03/2007", # 109
     "01/03/2009",  # 110
     "01/03/2011",  # 111
     "01/03/2013",  # 112
     "01/03/2015"), 
     format = "%m/%d/%Y")  # 113

congress <- c(109:113)

set_congress <- Vectorize(function(d) { congress[between(d, start, end)] })
JSF$Congress <- set_congress(JSF$effectivedate)


# Subset both JSF and house_assignments by Congress
JSF_109 <- subset(JSF, Congress == 109)
JSF_110 <- subset(JSF, Congress == 110)
JSF_111 <- subset(JSF, Congress == 111)
JSF_112 <- subset(JSF, Congress == 112)

house_109 <- subset(house_assignments, Congress == 109)
house_110 <- subset(house_assignments, Congress == 110)
house_111 <- subset(house_assignments, Congress == 111)
house_112 <- subset(house_assignments, Congress == 112)

# Merge house assignments and JSF by Congress
house_merge_109 <- merge(JSF_109, house_109, by.x = "cd", by.y = "cd", all.x = TRUE)
house_merge_109 <- subset(house_merge_109,
                          !duplicated(house_merge_109$unique_transaction_id))
house_merge_110 <- merge(JSF_110, house_110, by.x = "cd", by.y = "cd", all.x = TRUE)
house_merge_110 <- subset(house_merge_110,
                          !duplicated(house_merge_110$unique_transaction_id))
house_merge_111 <- merge(JSF_111, house_111, by.x = "cd", by.y = "cd", all.x = TRUE)
house_merge_111 <- subset(house_merge_111,
                          !duplicated(house_merge_111$unique_transaction_id))
house_merge_112 <- merge(JSF_112, house_112, by.x = "cd", by.y = "cd", all.x = TRUE)
house_merge_112 <- subset(house_merge_112,
                          !duplicated(house_merge_112$unique_transaction_id))

# Turn each Congress into one big DF
house_JSF <- rbind(house_merge_109, house_merge_110, house_merge_111, house_merge_112)

# Code dummy variable, 1 for Approp or Armed Services, 0 else
house_JSF$Committee <- 0
#house_JSF$Committee == 1 <- house_JSF$committee == "Armed Services"  

house_JSF$Committee[house_JSF$committee == "Armed Services" | house_JSF$committee == "Appropriations"] <- 1
                                       
# Save it
save(house_JSF, file = "house_subcom_JSF.RData")

# Save the house merge dataframes by Congress
save(house_merge_109, house_merge_110, house_merge_111, house_merge_112, file = "house_merge_Congress.RData")


