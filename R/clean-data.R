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
JSF$effectivedate <- as.Date(JSF$effectivedate, "%m/%d/%Y")
JSF$last_modified_date <- as.Date(JSF$last_modified_date, "%m/%d/%Y")
JSF$signeddate <- as.Date(JSF$signeddate, "%m/%d/%Y")
JSF$currentcompletiondate <- as.Date(JSF$currentcompletiondate, "%m/%d/%Y")
JSF$ultimatecompletiondate <- as.Date(JSF$ultimatecompletiondate, "%m/%d/%Y")
JSF$lastdatetoorder <- as.Date(JSF$lastdatetoorder, "%m/%d/%Y")

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

## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

## Let's begin by creating a single network with no temporal variation
setDT(JSF)
setkey(JSF, "contractingofficeagencyid", "cd")

A <- JSF[CJ(unique(cd), unique(contractingofficeagencyid)),
             .N, by = .EACHI]

A <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
             timevar = "contractingofficeagencyid", direction = "wide")

rownames(A) <- A[,1]
A[,1] <- NULL
A <- as.matrix(A)

make_type <- function(adj)
     c(rep("actor", nrow(adj)), rep("group", ncol(adj)))

N <- network(A, directed=FALSE, bipartite=TRUE)

type <- make_type(A)

plot(N, pad=0, edge.col=grey[2], vertex.border=FALSE,
     vertex.cex=ifelse(type == "actor", 0.75, 0.5),
     vertex.col=ifelse(type == "actor", "red", "gray"))

## Create temporal slices by Congress
## Created as DF because Jason's magic requires data frames
## JWM: I believe these need to be corrected. WM: They did.
JSF109 <-# as.data.frame(
     subset(JSF, 
            effectivedate >= as.Date("01/03/2005", "%m/%d/%Y") &                      
                 effectivedate <= as.Date("01/02/2007", "%m/%d/%Y"))
#)
JSF110 <- #as.data.frame(
     subset(JSF, 
            effectivedate >= as.Date("01/03/2007", "%m/%d/%Y") &                      
                 effectivedate <= as.Date("01/02/2009", "%m/%d/%Y"))
#)
JSF111 <- #as.data.frame(
     subset(JSF, 
            effectivedate >= as.Date("01/03/2009", "%m/%d/%Y") &                      
                 effectivedate <= as.Date("01/02/2011", "%m/%d/%Y"))
#)
JSF112 <- #as.data.frame(
     subset(JSF, 
            effectivedate >= as.Date("01/03/2011", "%m/%d/%Y") &                      
                 effectivedate <= as.Date("01/02/2013", "%m/%d/%Y"))
#)



## -----------------------------------------------------------------------------
## Create Adjacency Matrices
## A is 109, B is 110, C is 111, D is 112 (Congresses)
## -----------------------------------------------------------------------------

JSFadj <- list(JSF109, JSF110, JSF111, JSF112)

make_adjacency <- function(dta)
{
    A <- dta[CJ(unique(JSF$contractingofficeagencyid), unique(JSF$cd)),
             .N, by = .EACHI]
    out <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
            timevar = "contractingofficeagencyid", direction = "wide")
    out <- 1 * (out > 0)
}

JSFadj <- lapply(JSFadj, make_adjacency)
names(JSFadj) <- LETTERS[1:4]


## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

JSFnets <- lapply(JSFadj, network, directed=FALSE, bipartite=TRUE)

## Save networks for further processing.
save(JSFadj, JSFnets, file="data/JSF-networks.RData", compress="bzip2")
