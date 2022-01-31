##############################################################################
#
#                   Mixed effect model analysis
#                     Pierre Masselot
#
##############################################################################

#-------------------------------
# Prepare data for prediction
#-------------------------------

# Create grid for all Stringency Index prediction at all cities
datapred <- expand.grid(StringencyIndex = si_grid, id = cities$id)

# Add location coordinates to the prediction data.frame
datapred$cities_lon <- cities$cities_lon[match(datapred$id, cities$id)]
datapred$cities_lat <- cities$cities_lat[match(datapred$id, cities$id)]

# Add null values of confounders
datapred$dow <- 1
datapred$Greenness <- 0
datapred$BuiltUp <- 0

# Append it to the datatab
datainla <- rbind(datatab[,c("id", "StringencyIndex", 
  "cities_lon", "cities_lat", "dow", "Greenness", "BuiltUp")], 
  datapred)

# Number of prediction points
npred <- nrow(datapred)

#-------------------------------
# Prepare useful variables
#-------------------------------

# Spline expansion of Stringency Index
SIspline <- ns(datainla$StringencyIndex, df = sidf)
colnames(SIspline) <- sprintf("ns%i", 1:sidf)

# City indices for INLA
datainla$id_dow <- as.numeric(datainla$id)
datainla[,sprintf("id_ns%i", 1:sidf)] <- as.numeric(datainla$id)

#-------------------------------
# Prepare Spatial model
#-------------------------------

# Create mesh for SPDE
mesh <- inla.mesh.2d(cities[,3:4], max.edge = c(5, 10))

# Create covariance function as a penalized complexity matern
spde.spatial = inla.spde2.pcmatern(mesh = mesh, 
  alpha = 2,
  prior.range = c(30, 0.5), 
  prior.sigma = c(5, 0.01))

# Create linear combination matrix A
spline_A <- apply(SIspline, 2, function(x){
  inla.spde.make.A(mesh, 
    loc = data.matrix(datainla[,c("cities_lon", "cities_lat")]),
    weights = x)
})

# Creat spde index
spde_index <- lapply(sprintf("spatial.ns%s", 1:sidf), inla.spde.make.index,
  n.spde = spde.spatial$n.spde)

#-------------------------------
# Prepare INLA formula
#-------------------------------

# Constraint on DOW
Ac <- matrix(c(1, rep(0, 6)), nrow = 1)

# Prepare formula
fixed_part <- paste(colnames(SIspline), collapse = " + ")
spde_part <- paste(sprintf("f(spatial.ns%i, model = spde.spatial)", 1:sidf), 
  collapse = " + ")

inla_form <- as.formula(
  sprintf("y ~ -1 + %s + %s + Greenness + BuiltUp + 
      f(dow, model = 'iid', group = id_dow, constr = F,
        extraconstr = list(A = Ac, e = 0))",
    fixed_part, spde_part)
)

#-------------------------------
# Prepare lincomb for fixed effect posterior
#-------------------------------

# Evaluate splines at Stringency Index grid
spl_grid <- as.data.frame(ns(si_grid, knots = attr(SIspline, "knots"), 
  Boundary.knots = attr(SIspline, "Boundary.knots")))
names(spl_grid) <- sprintf("ns%i", 1:sidf)

# Prepare linear combination for fixed effect
fixed_lc <- do.call(inla.make.lincombs, spl_grid)
names(fixed_lc) <- sprintf("fixed%i", 1:ns)

######################################
# Loop on pollutants to apply INLA
######################################

# Create object to store results
reslist <- vector("list", np); names(reslist) <- pollutants
stacks <- vector("list", np); names(stacks) <- pollutants
fixed_pred <- array(NA, dim = c(ns, np, 3), 
  dimnames = list(si_grid, pollutants, c("mean", "low", "high")))
city_pred <- array(NA, dim = c(ns, nc, np, 3),
  dimnames = list(si_grid, cities$name, pollutants, c("mean", "low", "high")))

for (i in seq_len(np)){
  
  # print
  cat(i, "")
  
  # Choose outcome
  y <- datatab[,sprintf("Diff%s", pollutants[i])]
  
  # Creat stack object for INLA
  stack_obj <- inla.stack(data = list(y = c(y, rep(NA, npred))),
    A = c(spline_A, list(1)),
    effects = c(spde_index, list(cbind(SIspline, 
      datainla[,c("dow", "BuiltUp", "Greenness", "id_dow", 
        sprintf("id_ns%i", 1:sidf))]))),
    tag = "estim")
  
  #----------------------
  # Fit INLA model
  #----------------------
  reslist[[i]] <- inla(inla_form,
    data = inla.stack.data(stack_obj), 
    control.predictor = list(A = inla.stack.A(stack_obj), compute = T),
    lincomb = fixed_lc)
  
  # Keep the stack object
  stacks[[i]] <- stack_obj
  
  #----------------------
  # Extract curves
  #----------------------
  
  # fixed effect curve with CI
  fixed_pred[,i,] <- as.matrix(reslist[[i]]$summary.lincomb.derived[,
    c("mean", "0.025quant", "0.975quant")])
  
  # City specific predictions
  fits <- reslist[[i]]$summary.fitted.values[
    inla.stack.index(stack_obj, "estim")$data, 
    c("mean", "0.025quant", "0.975quant")][-(1:n),]
  fitsarr <- do.call(abind, 
    c(split(fits, rep(1:nc, each = ns)), list(along = 3)))
  city_pred[,,i,] <- aperm(fitsarr, c(1,3,2))
}
