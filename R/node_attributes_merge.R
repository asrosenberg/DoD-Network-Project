### Now we're going to merge in node attributes to house_JSF data

### Going to do this by Congress for simplicity
load("house_merge_Congress.RData")
load("node_attributes.RData")


# Make the CD format the same for node attributes data
attributes_109$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", attributes_109$cd, perl=TRUE)
attributes_110$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", attributes_110$cd, perl=TRUE)
attributes_111$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", attributes_111$cd, perl=TRUE)
attributes_112$cd <- gsub("([A-Z]{2})0([1-9])", "\\1\\2", attributes_112$cd, perl=TRUE)

### Un-log all of the contracts and contributions
final_JSF_109$ln_contrib <- exp(final_JSF_109$ln_contrib)
final_JSF_109$ln_contracts <- exp(final_JSF_109$ln_contracts)

### Un-log all of the contracts and contributions
final_JSF_110$ln_contrib <- exp(final_JSF_110$ln_contrib)
final_JSF_110$ln_contracts <- exp(final_JSF_110$ln_contracts)

### Un-log all of the contracts and contributions
final_JSF_111$ln_contrib <- exp(final_JSF_111$ln_contrib)
final_JSF_111$ln_contracts <- exp(final_JSF_111$ln_contracts)

### Un-log all of the contracts and contributions
final_JSF_112$ln_contrib <- exp(final_JSF_112$ln_contrib)
final_JSF_112$ln_contracts <- exp(final_JSF_112$ln_contracts)

# Let's try merging the Congresses
final_JSF_109 <- merge(house_merge_109, attributes_109, by.x = "cd", all.x = TRUE)
final_JSF_110 <- merge(house_merge_110, attributes_110, by.x = "cd", all.x = TRUE)
final_JSF_111 <- merge(house_merge_111, attributes_111, by.x = "cd", all.x = TRUE)
final_JSF_112 <- merge(house_merge_112, attributes_112, by.x = "cd", all.x = TRUE)

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



# Save it
save(final_JSF, final_JSF_109, final_JSF_110, final_JSF_111, final_JSF_112, file = "final_JSF_data.RData")