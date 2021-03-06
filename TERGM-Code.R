## =============================================================================
## Bipartite ERGM Example in Networks Package
## =============================================================================

## Load libraries
require(data.table)
require(network)
require(lattice)
require(statnet)
require(texreg)
require(xergm)

## Load data
JSF <- read.csv("~/Dropbox/Archive/Spring2015/Networks2/DoD-Contracts-Network-Project/JSF.csv.bz2")
JSF <- read.csv("JSF.csv.bz2")

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

## Sample some observations
# JSF<- JSF[sample(1:nrow(JSF), 500,replace=FALSE),]

## Add NA's to cells where we have blank values

JSF[JSF==""]  <- NA

## Let's see how many NAs we have for CD
summary(is.na(JSF$congressionaldistrict))
summary(is.na(JSF$contractingofficeid))

## Take NA's out

completeFun <- function(data, desiredCols) {
     completeVec <- complete.cases(data[, desiredCols])
     return(data[completeVec, ])
}
JSF <- completeFun(JSF, 3)

## Create temporal slices by Congress
## Created as DF because Jason's magic requires data frames
JSF108 <- as.data.frame(subset(JSF, fiscal_year > 2002 & fiscal_year < 2006))
JSF109 <- as.data.frame(subset(JSF, fiscal_year > 2005 & fiscal_year < 2008))
JSF110 <- as.data.frame(subset(JSF, fiscal_year > 2006 & fiscal_year < 2010))
JSF111 <- as.data.frame(subset(JSF, fiscal_year > 2008 & fiscal_year < 2012))



##############################################################
## For each slice, do Jason's magic to get adjacency matrices
## A is 108, B is 109, C is 110, D is 111 (Congresses)
##############################################################

setDT(JSF108)
setkey(JSF108, "contractingofficeid", "congressionaldistrict")

## data.table magic to create the adjacency matrix
A <- JSF108[CJ(unique(contractingofficeid), unique(congressionaldistrict)),
            .N, by = .EACHI]

A <- reshape(as.data.frame(A), v.names = "N", idvar = "congressionaldistrict",
             timevar = "contractingofficeid", direction = "wide")

dim(A)

setDT(JSF109)
setkey(JSF109, "contractingofficeid", "congressionaldistrict")

## data.table magic to create the adjacency matrix
B <- JSF109[CJ(unique(contractingofficeid), unique(congressionaldistrict)),
            .N, by = .EACHI]


B <- reshape(as.data.frame(B), v.names = "N", idvar = "congressionaldistrict",
             timevar = "contractingofficeid", direction = "wide")

dim(B)

setDT(JSF110)
setkey(JSF110, "contractingofficeid", "congressionaldistrict")

## data.table magic to create the adjacency matrix
C <- JSF110[CJ(unique(contractingofficeid), unique(congressionaldistrict)),
            .N, by = .EACHI]


C <- reshape(as.data.frame(C), v.names = "N", idvar = "congressionaldistrict",
             timevar = "contractingofficeid", direction = "wide")

dim(C)

setDT(JSF111)
setkey(JSF111, "contractingofficeid", "congressionaldistrict")

## data.table magic to create the adjacency matrix
D <- JSF111[CJ(unique(contractingofficeid), unique(congressionaldistrict)),
         .N, by = .EACHI]

D <- reshape(as.data.frame(D), v.names = "N", idvar = "congressionaldistrict",
             timevar = "contractingofficeid", direction = "wide")

dim(D)

###############################################################################
## Now create make_type function for bipartite network
## Put row names on each DF (for each congress)
## Then put them into a list
################################################################################
make_type <- function(adj)
    c(rep("actor", nrow(adj)), rep("group", ncol(adj)))

N <- network(A, directed=TRUE, bipartite=TRUE)

type <- make_type(A)

rownames(A) <- A[,1]
rownames(B) <- B[,1]
rownames(C) <- C[,1]
rownames(D) <- D[,1]


JSF_list <- list(A, B, C, D)

## Try to get rid of CDs and Agencies that don't show up in particular slices
A <- A[,colSums(A[, 3:31]) > 0]
A <- A[rowSums(A[, 3:31]) > 0]

###############################################################################
## Create M and N which are empty lists
## M will be a list of matricies (one for each Congress)
## N will be the same, but just for networks
## then plot them
################################################################################


M <- list(length(JSF_list))
N <- list(length(M))
for (i in 1:length(JSF_list)){
     M[[i]] <- as.matrix(JSF_list[[i]])
     N[[i]] <- network(M[[i]], directed = TRUE, bipartite = TRUE)
}

par(mfrow = c(2, 2), mar = c(0, 0, 1, 0))
for (i in 1:length(N)) {
     plot(N[[i]], main = paste("t =", i),
          usearrows = TRUE, edge.col = "grey50")
}


################################################################################
################################################################################
## M is the list of 4 matricies for each time slice
## Let's look at the giant components of each and hopefully the CDs are the same
## We're going to subset each congress by CDs and agencies with  > 0 edges
################################################################################
################################################################################

# Turn A, B, C, D into tables and look at frequencies

table(colSums(A[, 2:31]))
table(colSums(B[, 2:12]))
table(colSums(C[, 2:17]))
table(colSums(D[, 2:19]))

# Starting with A, it makes sense to only look at Agencies with > 3 edges
A <- A[,colSums(A[, 2:31]) > 3]
A_matrix <- as.matrix(A)

# Then turn our new A into a Giant Component network
# Check the densities
GC_108 <- network(A_matrix, directed = FALSE, bipartite = TRUE)
network.density(GC_108)
network.density(N[[1]])

# Plot the new 108 congress network next to the old one
par(mfrow = c(1,2))
plot(GC_108, main = "Giant Component for 108 Congress",
     usearrows = FALSE, edge.col = "gray", vertex.cex=ifelse(type == "actor", 0.5, 0.75),
     vertex.col=ifelse(type == "actor", "black", "red"))
plot(N[[1]], main = "Original 108 Congress Network",
     usearrows = FALSE, edge.col = "gray", vertex.cex=ifelse(type == "actor", 0.5, 0.75),
     vertex.col=ifelse(type == "actor", "black", "red"))

A_matrix_rows <- A_matrix[, 2:17]
table(rowSums(A_matrix_rows))
A_matrix_rows <- A_matrix_rows[rowSums(A_matrix_rows)]

sort(table(A$contractingofficeid))
sort(table(JSF108$congressionaldistrict))
plot(JSF108$congressionaldistrict)

A <- as.data.frame(A)
A <- A[,colSums(A[,2:33]) > 0]

## Convert to network
rownames(A) <- A[,1]
A[,1] <- NULL
A <- as.matrix(A)

## Plot as bipartite network (nodes = agencies & CDs, ties = PIID)
make_type <- function(adj)
     c(rep("actor", nrow(adj)), rep("group", ncol(adj)))

N <- network(A, directed=TRUE, bipartite=TRUE)

type <- make_type(A)

plot(N,
     main = "Bipartite Network of Federal Agencies and Congressional Districts",
     pad=0,
     edge.col='gray',
     vertex.border=FALSE,
     vertex.cex=ifelse(type == "actor", 0.75, 1.5),
     vertex.col=ifelse(type == "actor", "gray", "red"))

legend("topright",
       c("Federal Agencies","Congresional Districts"),
       pch=c(19,19),
       col=c("red","gray"),
       cex = 0.60)

## Weight edges by contract $ value? or volume?

## Add vertex attributes to the network
attr <- as.numeric(JSF$dollarsobligated)
set.vertex.attribute(N, names(attr), attr)


## Now run Bipartite ERGM on our two-mode network
require(ergm)

?ergm.terms

# bipartite ergm terms:
# b1factor, b2factor (main effect of a factor attribute)
# b1degree, b2degree (degree)
# b1star, b2star (k-stars)
# gwb1degree, gwb2degree (geometrically-weighted degree distribution)
# b1concurrent, b2concurrent (concurrent node count)


## A simple bipartite ergm
mod0 <- ergm(GC_108 ~ edges, verbose=TRUE,
             control = control.ergm(MCMC.samplesize = 1000))

gof0 <- gof(mod0)
plot(gof0)

# Same basic ergm but with pseudolikelihood estimation
mod1 <- ergm(GC_108 ~ edges, verbose=TRUE, estimate = "MPLE")

gof1 <- gof(mod1)
plot(gof1)

par(mfrow=c(1,3))

plot(gof0)

summary(mod0)

## Basic ERGM on data from the 108 congress
mod1 <- ergm(GC_108 ~ edges + b1star(1:2) + b2star(1:2), verbose=TRUE,
             control = control.ergm(MCMC.samplesize = 10000), MCMC.burnin=100000)

mod1 <- ergm(GC_108 ~ edges + b1star(1:2) + b2star(1:2), verbose=TRUE, estimate = "MPLE")
summary(mod1)

gof1 <- gof(mod1)

par(mfrow=c(1,3))

plot(gof1)

## Kitchen Sink Model for endogenous network effects
mod2 <- ergm(N ~ edges +
                     b1degree() +
                     b2degree() +
                     b1degreerange() +
                     b2degreerange() +
                     b1mindegree() +
                     b2mindegree() +
                     b1star() +
                     b2star() +
                     gwb1degree() +
                     gwb2degree(),
                     verbose=TRUE,
                     control = control.ergm(MCMC.samplesize = 1000))

summary(mod2)

gof1 <- gof(mod2)

par(mfrow=c(1,3))

plot(gof2)



## Estimate a bipartite TERGM

## SPEND LOTS OF TIME MAKING SURE YOU PRE-PROCESS CORRECTLY!


## -----------------------------------------------------------------------------
## Bipartite ERGMs on Orginal Data
## -----------------------------------------------------------------------------

ctrl <- control.ergm(
    MCMC.interval=1000,
    MCMC.burnin=200000,
    MCMC.samplesize=100000,
    MCMLE.maxit=10)

plot(summary(N[[1]] ~ b1degree(1:20)))
plot(summary(N[[1]] ~ b2degree(1:20)))

mod1 <- ergm(N[[1]] ~ edges, control=ctrl, verbose=TRUE)
summary(mod1)

DECAY <- seq(1, 4, by=0.2)
RESULT <- matrix(0, ncol=2, nrow=length(DECAY))

for (i in 1:length(DECAY)) {
    decay <- DECAY[i]
    RESULT[i,] <- try(coef(ergm(N[[1]] ~ edges
                                ## + gwb1degree(3.0, fixed=TRUE)
                                + gwb2degree(decay, fixed=TRUE),
                                verbose=TRUE, control=ctrl)))
}


## > RESULT
##            [,1]       [,2]
##  [1,]  1.517632  -6.944632
##  [2,]  1.578917  -4.785641
##  [3,]  2.045375  -9.890707
##  [4,]  2.466362  -8.483947
##  [5,]  2.927742  -9.103538
##  [6,]  3.434409 -10.359861
##  [7,]  3.776188  -9.870859
##  [8,]  4.276437 -10.577873
##  [9,]  4.666221 -10.473028
## [10,]  5.176068 -10.507367
## [11,]  5.622305 -10.716358
## [12,]  6.284993 -11.065329
## [13,]  7.024273 -11.621657
## [14,]  7.950342 -12.408078
## [15,]  9.117513 -13.468616
## [16,] 10.530569 -14.883984
