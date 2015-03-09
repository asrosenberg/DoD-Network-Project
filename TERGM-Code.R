## =============================================================================
## Bipartite ERGM Example in Networks Package
## =============================================================================

## Load libraries
require(data.table)
require(network)
require(lattice)
require("statnet")
require("texreg")
require("xergm")

## Load data
JSF <- read.csv("~/Downloads/JSF.csv.bz2")

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
D <- JSF[CJ(unique(contractingofficeid), unique(congressionaldistrict)),
         .N, by = .EACHI]

D <- reshape(as.data.frame(D), v.names = "N", idvar = "congressionaldistrict",
             timevar = "contractingofficeid", direction = "wide")

dim(D) 

###############################################################################
## Now creat make_type function for bipartite network
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

##### M is the list of 4 matricies for each time slice




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
mod0 <- ergm(N ~ edges, verbose=TRUE, 
             control = control.ergm(MCMC.samplesize = 1000))

gof0 <- gof(mod0)

par(mfrow=c(1,3))

plot(gof0)

summary(mod0)

## Another model
mod1 <- ergm(N ~ edges + b1degree(3), verbose=TRUE, 
             control = control.ergm(MCMC.samplesize = 1000))

summary(mod1)

gof1 <- gof(mod1)

par(mfrow=c(1,3))

plot(gof1)


## Save work
save(N, file = "bipartite_JSF_network")

## Estimate a bipartite TERGM

## SPEND LOTS OF TIME MAKING SURE YOU PRE-PROCESS CORRECTLY!



