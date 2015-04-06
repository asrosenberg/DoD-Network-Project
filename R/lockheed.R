###############################################################################
## Lockheed Martin Campaign Contributions to Congress: 2006-2012
##
###############################################################################

setwd("~/Dropbox/GitHub/DoD-Network-Project/data")

lockheed <- load("~/Dropbox/GitHub/DoD-Network-Project/data/lockheed.csv")

lockheed <- subset(lockheed, select=c("cycle",
                                      "date",
                                      "contributor_name",
                                      "recipient_name",
                                      "recipient_party",
                                      "recipient_state",
                                      "recipient_state_held",
                                      "district",
                                      "district_held",
                                      "amount",
                                      "committee_name",
                                      "seat_status",
                                      "seat_result"))

lockheed2006 <- subset(lockheed, cycle == 2006) 

lockheed2008 <- subset(lockheed, cycle == 2008) 

lockheed2010 <- subset(lockheed, cycle == 2010) 

lockheed2012 <- subset(lockheed, cycle == 2012) 

