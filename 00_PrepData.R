################################################################################
#
#                   MCC AP-Lockdown
#                    Tidy data
#
################################################################################

#-------------------------------
# Packages
#-------------------------------

# To install the INLA package : 
# install.packages("INLA",repos=c(getOption("repos"),
#   INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(splines)
library(abind)
library(data.table);library(fda); library(maps);library(RColorBrewer);library(ggplot2)

#-------------------------------
# Parameters
#-------------------------------

# Responsa variables
pollutants <- c("NO2", "O3", "PM25", "PM10")
cams_model <- "INERIS_CAMS_COP_066"

# Predictors and confounders
oxcgrt_policies <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "H1")
other_var <- c("name", "Date", "id", "country", "cities_lon", "cities_lat",
  "Population", "Greenness", "Eurostat_Deathrate", "WB_Deathrate", 
  "BuiltUp", "StringencyIndex")

# OXCGRT Stringency Index grid for prediction
si_grid <- 1:100
ns <- length(si_grid)

# Number of degrees of freedom for spline
sidf <- 4

# Number of simulations for Excess Mortality CI
nsim <- 1000

#-------------------------------
# Read full dataset
#-------------------------------

readdata <- readRDS("data_50cities.RDS")

#-------------------------------
# Select variables
#-------------------------------

# Select the columns in readdata
col_inds <- c(
  unlist(sapply(other_var, grep, names(readdata))),
  unlist(sapply(oxcgrt_policies, grep, names(readdata)))
) 

datatab <- readdata[,col_inds]

#----- Add the pollutant differences
for (i in seq_along(pollutants)){
  # Add pollutant series to data table
  datatab[,sprintf("%s_%s_COVID", cams_model, pollutants[i])] <- 
    readdata[,sprintf("%s_%s_COVID", cams_model, pollutants[i])]
  datatab[,sprintf("%s_%s_BAU", cams_model, pollutants[i])] <- 
    readdata[,sprintf("%s_%s_BAU", cams_model, pollutants[i])]
  
  # Add the difference
  datatab[,sprintf("Diff%s", pollutants[i])] <- 
    readdata[,sprintf("%s_%s_COVID", cams_model, pollutants[i])] - 
    readdata[,sprintf("%s_%s_BAU", cams_model, pollutants[i])]
}

#-------------------------------
# Clean data
#-------------------------------

# Remove NAs in the studied
cc <- complete.cases(datatab[,c("StringencyIndex", 
  sprintf("Diff%s", pollutants))])
datatab <- datatab[cc,]

# Reorder data
datatab <- datatab[with(datatab, order(id, Date)),]

# Create day of week data
datatab$dow <- as.numeric(factor(weekdays(datatab$Date)))

#-------------------------------
# Create city data.frame
#-------------------------------

cities <- unique(datatab[,c("name", "id", "cities_lon", "cities_lat", "country",
  "Greenness", "Population", "Eurostat_Deathrate", "WB_Deathrate")])

# We use Eurostat deathrate except when missing we use World Bank death rate
cities$DeathRate <- cities$Eurostat_Deathrate
cities[is.na(cities$DeathRate),"DeathRate"] <- 
  cities[is.na(cities$DeathRate),"WB_Deathrate"]
cities[,c("WB_Deathrate", "Eurostat_Deathrate")] <- NULL

# Those are annual death rates, thus we have to divide them by 365 to have daily
cities$DeathRate <- cities$DeathRate / 365.25

# Order cities
cities <- cities[order(cities$id),]

#-------------------------------
# Useful object
#-------------------------------

# Total number of data
n <- nrow(datatab)

# Number of pollutant
np <- length(pollutants)

# Number of considered policies
ncomp <- length(oxcgrt_policies)

# Number of cities left
nc <- nrow(cities)