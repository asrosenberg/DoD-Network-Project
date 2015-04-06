# 109-112 Congresses
# Appropriations, Armed Services

setwd("~/Dropbox/DoD/data")

house_assignments <- read.csv("stewart_woon.csv", header = TRUE)
colnames(house_assignments) <- NA
cols <- as.matrix(house_assignments[1,])
colnames(house_assignments) <- cols
house_assignments <- house_assignments[2:9255,]

#rownames(house_assignments) <- NULL



house_assignments <- subset(house_assignments, Congress == "109" & Congress == "110"
                           & Congress == "111"
                           & Congress == "112")