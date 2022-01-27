#####################################################
#
# Estimates of excess mortality
#
#####################################################

#---------------------------------
# Load data
#---------------------------------

# RRs from literature
# NO2: Meng et al. (2021)
# O3: Vicedo_Cabrera et al. (2020)
# PM2.5 and PM10: Liu et al. (2019)
# All for lag 0-1
RR10 <- matrix(c(1 + 0.46 / 100, 1 + 0.36 / 100, 1 + 0.57 / 100, 
    1.0018, 1.0012, 1.0024,
    1 + 0.68 / 100, 1 + 0.59 / 100, 1 + 0.77 / 100, 
    1 + 0.44 / 100, 1 + 0.39 / 100, 1 + 0.50 / 100),
  nrow = 4, ncol = 3, byrow = T, 
  dimnames = list(c("NO2", "O3", "PM25", "PM10"), c("Est", "low", "high"))
  )
coefs <- log(RR10) / 10

#---------------------------------
# Compute Excess Mortality
#---------------------------------

# Object to store results
excessMort <- BAUmort <- matrix(NA, nc, np * 3, 
  dimnames = list(cities$id, sprintf("ExcDeath_%s_%s", 
    rep(pollutants, each = 3), rep(c("mean", "low", "high"), np))))

simres <- simBAU <- vector("list", np)
for(p in seq(np)){

  #----- Compute excess mortality
    
  # Retrieve pollutant excess series
  excess_series <- split(datatab[,sprintf("Diff%s", pollutants[p])],
    droplevels(datatab$id))
  # Estimate daily increase
  RRseries <- lapply(excess_series, "*", coefs[p,1])
  # Estimate excess mortality
  mortSeries <- Map(function(RR, pop, dr) dr * pop * (1 - exp(-RR)), 
    RRseries, cities$Population, cities$DeathRate)
  # Total excess mortality
  excessMort[,(p - 1) * 3 + 1] <- sapply(mortSeries, sum)
  
  #----- Excess mortality in BAU scenario
  
  # Retrieve BAU series
  excess_series_BAU <- split(
    datatab[,sprintf("%s_%s_BAU", cams_model, pollutants[p])],
    droplevels(datatab$id))
  # Estimate daily increase
  RRseries_BAU <- lapply(excess_series_BAU, "*", coefs[p,1])
  # Estimate excess mortality
  mortSeries_BAU <- Map(function(RR, pop, dr) dr * pop * (1 - exp(-RR)), 
    RRseries_BAU, cities$Population, cities$DeathRate)
  # Total excess mortality
  BAUmort[,(p - 1) * 3 + 1] <- sapply(mortSeries_BAU, sum)

  #----- Simulate for confidence intervals
  sdcoef <- (coefs[p,1] - coefs[p,2]) / 1.96
  simcoefs <- rnorm(nsim, coefs[p,1], sdcoef)
  simres[[p]] <- simBAU[[p]] <- matrix(NA, nc, nsim)
  for (s in 1:nsim){
    RRseries <- lapply(excess_series, "*", simcoefs[s])
    # Estimate excess mortality
    mortSeries <- Map(function(RR, pop, dr) dr * pop * (1 - exp(-RR)), 
      RRseries, cities$Population, cities$DeathRate)
    simres[[p]][,s] <- sapply(mortSeries, sum)
    
    # And now for BAU
    RRseries_BAU <- lapply(excess_series_BAU, "*", simcoefs[s])
    mortSeries_BAU <- Map(function(RR, pop, dr) dr * pop * (1 - exp(-RR)), 
      RRseries_BAU, cities$Population, cities$DeathRate)
    simBAU[[p]][,s] <- sapply(mortSeries_BAU, sum)
  }
  
  # Compute confidence intervals
  excessMort[,(p - 1) * 3 + 2:3] <- t(apply(simres[[p]], 1, 
    quantile, c(.025, .975), na.rm = T))
  BAUmort[,(p - 1) * 3 + 2:3] <- t(apply(simBAU[[p]], 1, 
    quantile, c(.025, .975), na.rm = T))
}

# Total
totalExcessMort <- apply(excessMort, 2, sum, na.rm = T)
totalBAUmort <- apply(BAUmort, 2, sum, na.rm = T)

#----- Tidy results -----

# Excess mortality
excessMort <- data.frame(cities[,c(1:2,7:8)], excessMort)
excessMort$name <- as.character(excessMort$name)
totrow <- data.frame("Total", "", sum(cities$Population), NA, t(totalExcessMort))
names(totrow) <- names(excessMort)
excessMort <- rbind(excessMort, totrow)

# BAU attributable mortality
BAUmort <- cbind(cities[,c(1:2,7:8)], BAUmort)
BAUmort$name <- as.character(BAUmort$name)
totBAU <- data.frame("Total", "", sum(cities$Population), NA, t(totalBAUmort))
names(totBAU) <- names(BAUmort)
BAUmort <- rbind(BAUmort, totBAU)

