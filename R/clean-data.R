## =============================================================================
## Data Cleaning for JSF Networks
## =============================================================================

## Load libraries
require(data.table)
require(network)

## NOTE: Set working directory in R to the root of the contracts project.
## setwd("~/jwm/research/contracts")

## Load data
JSF <- read.csv("data/JSF.csv.bz2")

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
                            "zipcode"))


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
## JWM: I beleive these need to be corrected.
JSF108 <- as.data.frame(subset(JSF, fiscal_year > 2002 & fiscal_year < 2006))
JSF109 <- as.data.frame(subset(JSF, fiscal_year > 2005 & fiscal_year < 2008))
JSF110 <- as.data.frame(subset(JSF, fiscal_year > 2006 & fiscal_year < 2010))
JSF111 <- as.data.frame(subset(JSF, fiscal_year > 2008 & fiscal_year < 2012))


## -----------------------------------------------------------------------------
## Create Adjacency Matrices
## A is 108, B is 109, C is 110, D is 111 (Congresses)
## -----------------------------------------------------------------------------

setDT(JSF108)
setDT(JSF109)
setDT(JSF110)
setDT(JSF111)

setkey(JSF108, "contractingofficeid", "congressionaldistrict")
setkey(JSF109, "contractingofficeid", "congressionaldistrict")
setkey(JSF110, "contractingofficeid", "congressionaldistrict")
setkey(JSF111, "contractingofficeid", "congressionaldistrict")

JSFadj <- list(JSF108, JSF109, JSF110, JSF111)

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
