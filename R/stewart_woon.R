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

house_assignments <- subset(house_assignments, committee == "Appropriations" | 
                                 committee == "Armed Services")

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
## change values "MS00" and "TX00"
JSF$cd <- JSF$congressionaldistrict
JSF$cd[which(JSF$congressionaldistrict == "MS00")] <- "MS04"
JSF$cd[which(JSF$congressionaldistrict == "TX00")] <- "TX12"

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




merge_JSF_house <- merge(JSF, house_assignments, by = c("cd", "Congress"))
