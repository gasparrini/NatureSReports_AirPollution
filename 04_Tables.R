#####################################################
#
# Tables
#
#####################################################

#------------------------
# Table A2: Stringency Index results
#------------------------

# Max level of StringencyIndex
SImax <- aggregate(StringencyIndex ~ id, data = datatab, max, na.rm = T)
names(SImax)[2] <- "MaxSI"

# High SI level for display
highSI <- 80

# Pretty estimates at 80% table
SIeffect <- city_pred[si_grid == highSI,,,1]
for (p in seq_len(np)){
  pcis <- city_pred[si_grid == highSI,,p,]
  SIeffect[,p] <- sprintf("%2.1f (%2.1f ; %2.1f)", pcis[,1], pcis[,2], pcis[,3])
}
tabSI <- cbind(cities[,c("id", "name", "country")], maxSI = SImax[,2], SIeffect)
tabSI[1:3] <- lapply(tabSI[1:3], as.character)

# Add fixed effect
pretty_fixed <- sprintf("%2.1f (%2.1f ; %2.1f)", 
  fixed_pred[si_grid == highSI,,1], fixed_pred[si_grid == highSI,,2], 
  fixed_pred[si_grid == highSI,,3])
tot_tabSI <- rbind(tabSI, c(id = "Total", name = "", country = "", 
  max(SImax[,2]), pretty_fixed))

write.table(tot_tabSI, file = "Results/TableA2.csv",
  row.names = F, col.names = T, sep = ",")

#------------------------
# Table 1
#------------------------

# Pretty excess mortality table
prettyMort <- excessMort["id"]
prettyBAU <- BAUmort["id"]
for (p in seq_len(np)){
  # Excess mortality
  pcols <- excessMort[,grep(pollutants[p], colnames(excessMort))]
  prettyMort[,sprintf("ExcessDeaths_%s", pollutants[p])] <- 
    sprintf("%2.1f (%2.1f ; %2.1f)", pcols[,1], pcols[,2], pcols[,3])
  
  # BAU mortality
  pcols <- BAUmort[,grep(pollutants[p], colnames(BAUmort))]
  prettyBAU[,sprintf("BAUDeaths_%s", pollutants[p])] <- 
    sprintf("%2.1f (%2.1f ; %2.1f)", pcols[,1], pcols[,2], pcols[,3])
}

# Totals
prettyMort[,"ExcessDeaths_Tot"] <- sprintf("%2.1f", 
  rowSums(excessMort[,grep("mean", colnames(excessMort))]))
prettyBAU[,"BAUDeaths_Tot"] <- sprintf("%2.1f", 
  rowSums(BAUmort[,grep("mean", colnames(BAUmort))]))

# Merge all info together
desc_tab <- Reduce(merge, list(
  cities[, c("id", "name", "country", "Population")],
  SImax,
  prettyMort[1:nc,1:(np + 1)]
))
desc_tab[1:3] <- sapply(desc_tab[1:3], as.character)
desc_tab <- rbind(desc_tab, 
  c(id = "Total", name = "", country = "", Population = sum(cities$Population), 
    MaxSI = max(SImax[,2]), prettyMort[nrow(prettyMort),1:np + 1]))

# Export
write.table(desc_tab, file = "Results/Table1.csv",
  row.names = F, col.names = T, sep = ",", quote = F)

#------------------------
# Table A4: Deaths under BAU scenario
#------------------------

tabA4 <- Reduce(merge, list(
  cities[, c("id", "name", "country")],
  prettyBAU[1:nc,1:(np + 1)]
))
tabA4$ExpectedDeaths <- round(cities$DeathRate * 
  as.numeric(diff(range(datatab$Date))) * cities$Population)
tabA4 <- rbind(tabA4,
  c(id = "Total", name = "", country = "",
    prettyBAU[nrow(prettyBAU),1:np + 1],
    ExpectedDeaths = round(sum(tabA4$ExpectedDeaths, na.rm = T))
  ))

write.table(tabA4, file = "Results/TableA4.csv",
  row.names = F, col.names = T, sep = ",", quote = F)