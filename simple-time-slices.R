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


## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

## Let's begin by creating a single network with no temporal variation
setDT(JSF)
setkey(JSF, "contractingofficeagencyid", "cd")

## The number of rows that should be in A below.
# N <- length(unique(JSF$cd)) * length(unique(JSF$contractingofficeagencyid))
# 
# A <- JSF[CJ(unique(contractingofficeagencyid), unique(cd)),
#          .N, by = .EACHI]
# 
# ## Stop everything if the rows in A do not match N.
# if (nrow(A) != N)
#      stop("The number of rows in A is not correct.")
# 
# ## Stop everything if A$N is all 0, means something broke.
# if (max(A$N) == 0)
#      stop("No observations recorded in A.")
# 
# A <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
#              timevar = "contractingofficeagencyid", direction = "wide")
# 
# rownames(A) <- A[,1]
# A[,1] <- NULL
# A <- as.matrix(A)
# colnames(A) <- gsub("^N.", "", colnames(A))
# 
# ## Verify the data
# max_idx    <- which(A == max(A), arr.ind = TRUE)
# max_office <- colnames(A)[max_idx[2]]
# max_cd     <- rownames(A)[max_idx[1]]
# if (max(A) != nrow(JSF[contractingofficeagencyid == max_office & cd == max_cd]))
#      stop("Maximum number of contracts in A does not match that in JSF")
# 
# ## Note that this adds an edge attribute, contracts, recording the number of
# ## contracts recorded between CD and office.
# 
# FullNet <- network(A, directed=FALSE, bipartite=TRUE, ignore.eval=FALSE,
#                    names.eval="contracts")
# 
# FullNet %v% "type" <- c(rep("cd", nrow(A)), rep("office", ncol(A)))
# 
# ## Plot FullNet
# plot(FullNet, displaylabels=FALSE, pad=0, edge.col="gray", vertex.border=FALSE,
#      vertex.cex=ifelse(FullNet %v% "type" == "cd", 1, 1.75),
#      vertex.col=ifelse(FullNet %v% "type" == "cd", "black", "red"),
#      main = "Bipartite Network of JSF Contracts: FY 2001 - FY 2014")
# legend("topright", legend = c("Agencies", "CDs"), col = c("red", "black"), 
#        pch = 19)



## Create temporal slices by Congress. JWM: Notice I added the 108th and 113th
## congresses because many contracts had dates earlier than the 109th and there
## was one in the 113th.

## Now you can make more time slices here.
start <- as.Date(c(
     "01/03/2005", # 109
     "01/03/2006",  # 110
     "01/03/2007",  # 110
     "01/03/2008",  # 110
     "01/03/2009",  # 111
     "01/03/2010",  # 110
     "01/03/2011"),  # 112), 
     format = "%m/%d/%Y") 

end   <- as.Date(c(
     "01/03/2007", # 109
     "01/03/2008",  # 110
     "01/03/2009",  # 110
     "01/03/2010",  # 110
     "01/03/2011",  # 111
     "01/03/2012",  # 110
     "01/03/2013"),  # 112), 
     format = "%m/%d/%Y")  # 113

time_slices <- seq_len(length(start))

set_congress <- Vectorize(function(d) { time_slices[between(d, start, end)] })

# Jason, don't fuck with this.
JSF$congress <- 1
JSF$congress[JSF$effectivedate >= start[2] & JSF$effectivedate <= end[2]] <- 2
JSF$congress[JSF$effectivedate >= start[3] & JSF$effectivedate <= end[3]] <- 3
JSF$congress[JSF$effectivedate >= start[4] & JSF$effectivedate <= end[4]] <- 4
JSF$congress[JSF$effectivedate >= start[5] & JSF$effectivedate <= end[5]] <- 5
JSF$congress[JSF$effectivedate >= start[6] & JSF$effectivedate <= end[6]] <- 6
JSF$congress[JSF$effectivedate >= start[7] & JSF$effectivedate <= end[7]] <- 7

JSF$year <- cut(JSF$signeddate, "year")
uniq_year <- sort(unique(JSF$year))


## -----------------------------------------------------------------------------
## Create Adjacency Matrices for the 4 congresses of interest: 109--112
## -----------------------------------------------------------------------------

make_adjacency <- function(year_val, dta=JSF)
{
     dta <- dta[year == year_val]
     A <- dta[CJ(unique(JSF$contractingofficeagencyid), unique(JSF$cd)),
              .N, by = .EACHI]
     out <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
                    timevar = "contractingofficeagencyid", direction = "wide")
     rownames(out) <- out[,1]
     out[,1] <- NULL
     out <- as.matrix(out)
     colnames(out) <- gsub("^N.", "", colnames(out))
     
     
     out <- network(out, directed=FALSE, bipartite=TRUE)
     
     Dol <- dta[CJ(unique(JSF$contractingofficeagencyid), unique(JSF$cd)),
                .(dollars=sum(dollarsobligated)), by = .EACHI]
     Dol <- reshape(as.data.frame(Dol), v.names = "dollars", idvar = "cd",
                    timevar = "contractingofficeagencyid", direction = "wide")
     rownames(Dol) <- Dol[, 1]
     Dol[, 1] <- NULL
     Dol <- as.matrix(Dol)
     
     # comment next two lines for unlogged dollars
     Dol <- apply(Dol, 2, function(col) pmax(0, col)) # make negatives = 0
     Dol <- log1p(Dol) # log(x + 1)
     
     colnames(Dol) <- gsub("^N.", "", colnames(Dol))
     Dol[is.na(Dol)] <- 0
     
     tmp <- cbind(r=as.vector(row(Dol)),
                  c=as.vector(col(Dol))+max(row(Dol)),
                  dollars=as.vector(Dol))
     tmp <- tmp[!is.na(tmp[,3]),]
     
     ## Warning: There are negative dollar amounts.
     out %e% "dollars" <- tmp[,3]
     
     out
}

#JSFnets <- lapply(time_slices, make_adjacency, dta=JSF)
JSFnets <- lapply(uniq_year, make_adjacency, dta=JSF)
JSFadj <- lapply(JSFnets, as.matrix)
names(JSFadj) <- paste0("C", time_slices)


## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

#JSFnets <- lapply(JSFadj, network, directed=FALSE, bipartite=TRUE)


# -----------------------------------------------------------------------------
# Add covariates to FullNet
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(JSFnets, file="data/simple-time-slices.RData",
     compress="bzip2")
