## =============================================================================
## Data Cleaning for JSF Networks
## =============================================================================

## Load libraries
require(data.table)
require(network)

## NOTE: Set working directory in R to the root of the contracts project.
## setwd("~/jwm/research/contracts")

## Load data
JSF <- read.csv("data/JSF.csv.bz2", stringsAsFactors = FALSE)

## Just keep a few columns
JSF <- subset(JSF, select=c("agencyid",
                            "baseandalloptionsvalue",
                            "congressionaldistrict",
                            "contractingofficeagencyid",
                            "contractingofficeid",
                            "currentcompletiondate",
                            "dollarsobligated",
                            "dunsnumber",
                            "effectivedate",
                            "fiscal_year",
                            "fundingrequestingofficeid",
                            "mod_agency",
                            "mod_parent",
                            "modnumber",
                            "parentdunsnumber",
                            "piid",
                            "placeofperformancecongressionaldistrict",
                            "principalnaicscode",
                            "systemequipmentcode",
                            "ultimatecompletiondate",
                            "unique_transaction_id",
                            "zipcode",
                            "last_modified_date"))
JSF$last_modified_date <- as.Date(JSF$last_modified_date, "%m/%d/%Y")

## Add NA's to cells where we have blank values
JSF[JSF==""]  <- NA

## Let's see how many NAs we have for CD
table(is.na(JSF$congressionaldistrict))
table(is.na(JSF$contractingofficeid))

## Take NA's out
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}
JSF <- completeFun(JSF, 3)

## Create temporal slices by Congress
## Created as DF because Jason's magic requires data frames
## JWM: I beleive these need to be corrected. WM: They did.
JSF109 <- as.data.frame(subset(JSF, 
     last_modified_date >= as.Date("01/03/2005", "%m/%d/%Y") &                      
     last_modified_date <= as.Date("01/02/2007", "%m/%d/%Y")))
JSF110 <- as.data.frame(subset(JSF, 
     last_modified_date >= as.Date("01/03/2007", "%m/%d/%Y") &                      
     last_modified_date <= as.Date("01/02/2009", "%m/%d/%Y")))
JSF111 <- as.data.frame(subset(JSF, 
     last_modified_date >= as.Date("01/03/2009", "%m/%d/%Y") &                      
     last_modified_date <= as.Date("01/02/2011", "%m/%d/%Y")))
JSF112 <- as.data.frame(subset(JSF, 
     last_modified_date >= as.Date("01/03/2011", "%m/%d/%Y") &                      
     last_modified_date <= as.Date("01/02/2013", "%m/%d/%Y")))


## -----------------------------------------------------------------------------
## Create Adjacency Matrices
## A is 108, B is 109, C is 110, D is 111 (Congresses)
## -----------------------------------------------------------------------------

setDT(JSF109)
setDT(JSF110)
setDT(JSF111)
setDT(JSF112)

setkey(JSF109, "contractingofficeid", "congressionaldistrict")
setkey(JSF110, "contractingofficeid", "congressionaldistrict")
setkey(JSF111, "contractingofficeid", "congressionaldistrict")
setkey(JSF112, "contractingofficeid", "congressionaldistrict")

JSFadj <- list(JSF109, JSF110, JSF111, JSF112)

make_adjacency <- function(dta)
{
    A <- dta[CJ(unique(contractingofficeid), unique(congressionaldistrict)),
             .N, by = .EACHI]
    reshape(as.data.frame(A), v.names = "N", idvar = "congressionaldistrict",
            timevar = "contractingofficeid", direction = "wide")
}

JSFadj <- lapply(JSFadj, make_adjacency)
names(JSFadj) <- LETTERS[1:4]


## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

JSFnets <- lapply(JSFadj, network, directed=FALSE, bipartite=TRUE)

## Save networks for further processing.
save(JSFadj, JSFnets, file="data/JSF-networks.RData", compress="bzip2")
