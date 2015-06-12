## =============================================================================
## Data Cleaning for JSF Networks
## =============================================================================

## Load libraries
require(data.table)
require(network)

## Load data
JSF <- read.csv("data/JSF.csv.bz2", stringsAsFactors = FALSE)

## Just keep a few columns
JSF <- subset(JSF, select=c("agencyid",
                            "baseandalloptionsvalue",
                            "congressionaldistrict",
                            "contractingofficeagencyid",
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

# Make into R Date format
JSF$effectivedate          <- as.Date(JSF$effectivedate, "%m/%d/%Y")
JSF$last_modified_date     <- as.Date(JSF$last_modified_date, "%m/%d/%Y")
JSF$signeddate             <- as.Date(JSF$signeddate, "%m/%d/%Y")
JSF$currentcompletiondate  <- as.Date(JSF$currentcompletiondate, "%m/%d/%Y")
JSF$ultimatecompletiondate <- as.Date(JSF$ultimatecompletiondate, "%m/%d/%Y")
JSF$lastdatetoorder        <- as.Date(JSF$lastdatetoorder, "%m/%d/%Y")

## Subset even further
JSF <- subset(JSF, select=c("fiscal_year",
                            "contractingofficeagencyid", 
                            "congressionaldistrict", 
                            "mod_parent",
                            "dollarsobligated",
                            "principalnaicscode",
                            "systemequipmentcode")) 

## Let's remove the few remaining irrelvant territories
JSF <- subset(JSF, JSF$congressionaldistrict != "PR00") ## Sorry Puerto Rico!
JSF <- subset(JSF, JSF$congressionaldistrict != "GU00") ## Sorry Guam!
JSF <- subset(JSF, JSF$congressionaldistrict != "AS00") ## Sorry American Somoa!

## make a new congressional district field, 
## copy in old congresionaldistrict
## change values "MS00" and "TX00"
JSF$cd <- JSF$congressionaldistrict
JSF$cd[which(JSF$congressionaldistrict == "MS00")] <- "MS04"
JSF$cd[which(JSF$congressionaldistrict == "TX00")] <- "TX12"
JSF$cd[which(JSF$congressionaldistrict == "MT00")] <- "MT01"
JSF$cd[which(JSF$congressionaldistrict == "DC00")] <- "DC01"
JSF$cd[which(JSF$congressionaldistrict == "AK00")] <- "AK01"
JSF$cd[which(JSF$congressionaldistrict == "DE00")] <- "DE01"
JSF$cd[which(JSF$congressionaldistrict == "ND00")] <- "ND01"
JSF$cd[which(JSF$congressionaldistrict == "SE00")] <- "SD01"

## Add NA's to cells where we have blank values
JSF[JSF==""]  <- NA

## Let's see how many NAs we have for CD
table(is.na(JSF$cd))  # missingness here from multinational firms
table(is.na(JSF$contractingofficeagencyid))

## Take NA's out
completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}

JSF <- completeFun(JSF, "cd")
JSF <- completeFun(JSF, "contractingofficeagencyid")

## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

## Let's begin by creating a single network with no temporal variation
setDT(JSF)
setkey(JSF, "contractingofficeagencyid", "cd")

## The number of rows that should be in A below.
N <- length(unique(JSF$cd)) * length(unique(JSF$contractingofficeagencyid))

A <- JSF[CJ(unique(contractingofficeagencyid), unique(cd)),
         .N, by = .EACHI]

## Stop everything if the rows in A do not match N.
if (nrow(A) != N)
    stop("The number of rows in A is not correct.")

## Stop everything if A$N is all 0, means something broke.
if (max(A$N) == 0)
    stop("No observations recorded in A.")

A <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
             timevar = "contractingofficeagencyid", direction = "wide")

rownames(A) <- A[,1]
A[,1] <- NULL
A <- as.matrix(A)
colnames(A) <- gsub("^N.", "", colnames(A))

## Verify the data
max_idx    <- which(A == max(A), arr.ind = TRUE)
max_office <- colnames(A)[max_idx[2]]
max_cd     <- rownames(A)[max_idx[1]]
if (max(A) != nrow(JSF[contractingofficeagencyid == max_office & cd == max_cd]))
    stop("Maximum number of contracts in A does not match that in JSF")

## Note that this adds an edge attribute, contracts, recording the number of
## contracts recorded between CD and office.

FullNet <- network(A, directed=FALSE, bipartite=TRUE, ignore.eval=FALSE,
                   names.eval="contracts")

# Add vertices for both modes to account for complete universe of cases
FullNet <- add.vertices(FullNet, nv = 16, last.mode = TRUE)

FullNet<- add.vertices(FullNet, nv = 335, last.mode = FALSE)

FullNet %v% "type" <- c(rep("cd", FullNet$gal$bipartite), rep("office", FullNet$gal$n - FullNet$gal$bipartite))

FullNet

# plot FullNet
plot(FullNet,
     displayisolates = FALSE,
     displaylabels = TRUE,
     label.cex = 0.25,
     edge.col = "gray",
     edge.lwd = 1,
     vertex.cex = ifelse(FullNet %v% "type" == "cd", .70, 1.15),
     vertex.col = ifelse(FullNet %v% "type" != "cd", "darkred", "black"))


# vertex.col="hiv",
# vertex.sides="sex",
# vertex.cex=5,
# vertex.rot=-30,

# pdf("~/Dropbox/Academic_Conferences/POLNET_2015/Poster/spaghettiplot-20150612.pdf", 
#     width = 20,  height = 20, pointsize = 24, family = "Palatino") 
# dev.off()


## Create temporal slices by Congress. JWM: Notice I added the 108th and 113th
## congresses because many contracts had dates earlier than the 109th and there
## was one in the 113th.
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
JSF$congress <- set_congress(JSF$effectivedate)

## -----------------------------------------------------------------------------
## Create Adjacency Matrices for the 4 congresses of interest: 109--112
## -----------------------------------------------------------------------------

make_adjacency <- function(cong, dta=JSF)
{
    dta <- dta[cd == cong]
    A <- dta[CJ(unique(JSF$contractingofficeagencyid), unique(JSF$cd)),
             .N, by = .EACHI]
    out <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
                   timevar = "contractingofficeagencyid", direction = "wide")
    rownames(out) <- out[,1]
    out[,1] <- NULL
    out <- as.matrix(out)
    colnames(out) <- gsub("^N.", "", colnames(out))
    out
}

JSFadj <- lapply(109:113, make_adjacency, dta=JSF)
names(JSFadj) <- paste0("C", 109:113)


## -----------------------------------------------------------------------------
## Create List of Bipartite Networks by Time Slice
## -----------------------------------------------------------------------------

JSFnets <- lapply(JSFadj, network, directed=FALSE, bipartite=TRUE)

## -----------------------------------------------------------------------------
## Add covariates to FullNet
## -----------------------------------------------------------------------------

Dol <- JSF[CJ(unique(JSF$contractingofficeagencyid), unique(JSF$cd)),
           .(dollars=sum(dollarsobligated)), by = .EACHI]
Dol <- reshape(as.data.frame(Dol), v.names = "dollars", idvar = "cd",
               timevar = "contractingofficeagencyid", direction = "wide")
rownames(Dol) <- Dol[,1]
Dol[,1] <- NULL
Dol <- as.matrix(Dol)
colnames(Dol) <- gsub("^N.", "", colnames(Dol))
Dol[is.na(Dol)] <- 0

tmp <- cbind(r=as.vector(row(Dol)),
             c=as.vector(col(Dol))+max(row(Dol)),
             dollars=as.vector(Dol))
tmp <- tmp[!is.na(tmp[,3]),]

## Warning: There are negative dollar amounts.
FullNet %e% "dollars" <- tmp[,3]

## -----------------------------------------------------------------------------
## Principal NAICS codes for JSF firms
## -----------------------------------------------------------------------------

# create a vector of unique NAICS codes for contractors
JSF_naics <- as.vector(unique(JSF$principalnaicscode))

JSF_naics[21, 1] <- NA
JSF_naics[35, 1] <- NA
JSF_naics <- na.omit(JSF_naics) # 85 unique NAICS codes for JSF data
save(JSF_naics, file = "JSF_naics.RData")

## -----------------------------------------------------------------------------
## Save the Clean Data
## -----------------------------------------------------------------------------

save(JSF, FullNet, JSFadj, JSFnets, file = "JSF-networks.RData", compress = "bzip2")
