## Load libraries
require(data.table)
require(network)

#setwd("~/Dropbox/DoD/data")
load("final_JSF_data.RData")
## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

## Let's begin by creating a single network with no temporal variation
setDT(final_JSF)
setkey(final_JSF, "contractingofficeagencyid", "cd")

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

final_JSF$effectivedate <- as.Date(final_JSF$effectivedate, "%m/%d/%Y")

# Jason, don't fuck with this.
final_JSF$congress <- 1
final_JSF$congress[final_JSF$effectivedate >= start[2] & final_JSF$effectivedate <= end[3]] <- 2
final_JSF$congress[final_JSF$effectivedate >= start[3] & final_JSF$effectivedate <= end[5]] <- 3
final_JSF$congress[final_JSF$effectivedate >= start[4] & final_JSF$effectivedate <= end[4]] <- 4
final_JSF$congress[final_JSF$effectivedate >= start[5] & final_JSF$effectivedate <= end[5]] <- 5
final_JSF$congress[final_JSF$effectivedate >= start[6] & final_JSF$effectivedate <= end[6]] <- 6
final_JSF$congress[final_JSF$effectivedate >= start[7] & final_JSF$effectivedate <= end[7]] <- 7


## -----------------------------------------------------------------------------
## Create Adjacency Matrices for the 4 congresses of interest: 109--112
## -----------------------------------------------------------------------------

make_adjacency <- function(cong, dta=final_JSF)
{
     dta <- dta[congress == cong]
     A <- dta[CJ(unique(final_JSF$contractingofficeagencyid), unique(final_JSF$cd)),
              .N, by = .EACHI]
     out <- reshape(as.data.frame(A), v.names = "N", idvar = "cd",
                    timevar = "contractingofficeagencyid", direction = "wide")
     rownames(out) <- out[,1]
     out[,1] <- NULL
     out <- as.matrix(out)
     colnames(out) <- gsub("^N.", "", colnames(out))
     
     
     out <- network(out, directed=FALSE, bipartite=TRUE)
     
     Dol <- dta[CJ(unique(final_JSF$contractingofficeagencyid), unique(final_JSF$cd)),
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

final_JSFadj <- lapply(time_slices, make_adjacency, dta=final_JSF)
names(final_JSFadj) <- paste0("C", time_slices)


## -----------------------------------------------------------------------------
## Create Bipartite Networks
## -----------------------------------------------------------------------------

#JSFnets <- lapply(JSFadj, network, directed=FALSE, bipartite=TRUE)
final_JSFnets <- final_JSFadj

# -----------------------------------------------------------------------------
# Add node covariates to FullNet
# -----------------------------------------------------------------------------

for (i in 1:length(final_JSFnets)) {
     final_JSFnets[[i]] <- network(final_JSFnets[[i]]) 
     # turn each outcome net into a network object
}


   #  final_JSFnets[[i]] <- set.vertex.attribute(final_JSFnets[[i]], "ln_contracts", 
                                        ln_contracts)  # add as vertex attribute
    # final_JSFnets[[i]] <- set.vertex.attribute(final_JSFnets[[i]], "ln_contrib", 
                                        ln_contrib) # add as vertex attribute
     #final_JSFnets[[i]] <- set.vertex.attribute(final_JSFnets[[i]], "Committee", 
                                        Committee) # add as vertex attribute

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(final_JSFnets, file="final-time-slices.RData", compress="bzip2")

