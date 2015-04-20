### Now we're going to merge in node attributes to house_JSF data

### Going to do this by Congress for simplicity
load("house_merge_Congress.RData")
load("node_attributes.RData")
load("lockheed_all.RData")
load("lockheed_slices.RData")
load("comparative_advantage.Rdata")

### Now combine the lockheed, comparative advantage together by congress
final_attributes_109 <- merge(as.data.frame(dod_all), as.data.frame(lockheed2006), by.x = "cd", all.x = TRUE)
final_attributes_110 <- merge(as.data.frame(dod_all), as.data.frame(lockheed2008), by.x = "cd", all.x = TRUE)
final_attributes_111 <- merge(as.data.frame(dod_all), as.data.frame(lockheed2010), by.x = "cd", all.x = TRUE)
final_attributes_112 <- merge(as.data.frame(dod_all), as.data.frame(lockheed2012), by.x = "cd", all.x = TRUE)
save(final_attributes_109, final_attributes_110, final_attributes_111, final_attributes_112, file = "final_node_attributes.RData")


# Make the CD format the same for node attributes data
final_attributes_109$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", final_attributes_109$cd, perl=TRUE)
final_attributes_110$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", final_attributes_110$cd, perl=TRUE)
final_attributes_111$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", final_attributes_111$cd, perl=TRUE)
final_attributes_112$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", final_attributes_112$cd, perl=TRUE)


# Let's try merging the Congresses
final_JSF_109 <- merge(house_merge_109, final_attributes_109, by.x = "cd", all.x = TRUE)
final_JSF_110 <- merge(house_merge_110, final_attributes_110, by.x = "cd", all.x = TRUE)
final_JSF_111 <- merge(house_merge_111, final_attributes_111, by.x = "cd", all.x = TRUE)
final_JSF_112 <- merge(house_merge_112, final_attributes_112, by.x = "cd", all.x = TRUE)

# Make one dataframe 
final_JSF <- rbind(final_JSF_109, final_JSF_110, final_JSF_111, final_JSF_112)

# Make Dummy Again for committee
# Code dummy variable, 1 for Approp or Armed Services, 0 else
final_JSF_109$Committee <- 0
final_JSF_109$Committee[final_JSF_109$committee == "Armed Services" | final_JSF_109$committee == "Appropriations"] <- 1
final_JSF_110$Committee <- 0
final_JSF_110$Committee[final_JSF_110$committee == "Armed Services" | final_JSF_110$committee == "Appropriations"] <- 1
final_JSF_111$Committee <- 0
final_JSF_111$Committee[final_JSF_111$committee == "Armed Services" | final_JSF_111$committee == "Appropriations"] <- 1
final_JSF_112$Committee <- 0
final_JSF_112$Committee[final_JSF_112$committee == "Armed Services" | final_JSF_112$committee == "Appropriations"] <- 1


final_JSF$Committee <- 0
final_JSF$Committee[final_JSF$committee == "Armed Services" | final_JSF$committee == "Appropriations"] <- 1

# Rename contributions and comparative advantage columns
final_JSF_109$contracts <- final_JSF_109$dollars  
final_JSF_109$contrib <- final_JSF_109$amount 

final_JSF_110$contracts <- final_JSF_110$dollars 
final_JSF_110$contrib <- final_JSF_110$amount

final_JSF_111$contracts <- final_JSF_111$dollars 
final_JSF_111$contrib <- final_JSF_111$amount

final_JSF_112$contracts <- final_JSF_112$dollars
final_JSF_112$contrib <- final_JSF_112$amount 

final_JSF$contracts <- final_JSF$dollars
final_JSF$contrib <- final_JSF$amount 

# Save it
save(final_JSF, final_JSF_109, final_JSF_110, final_JSF_111, final_JSF_112, file = "final_JSF_data.RData")