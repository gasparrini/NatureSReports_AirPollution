#####################################################
#
# Linear mixed effect model of specific policies
#
#####################################################

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

# Creat spde index
policy_index <- inla.spde.make.index("spatial.poli", n.spde = spde.spatial$n.spde)
spline_index <- lapply(sprintf("spatial.ns%s", 1:sidf), inla.spde.make.index,
  n.spde = spde.spatial$n.spde)

# City indices for INLA
datatab$id_dow <- datatab$id_pol <-as.numeric(datatab$id)
datatab[,sprintf("id_ns%i", 1:sidf)] <- as.numeric(datatab$id)

#-------------------------------
# Prepare INLA formula
#-------------------------------

# Constraint on DOW
Ac <- matrix(c(1, rep(0, 6)), nrow = 1)

# Prepare formula
fixed_part <- paste(sprintf("ns%i", 1:sidf), collapse = " + ")
spde_part <- paste(sprintf("f(spatial.ns%i, model = spde.spatial)", 1:sidf), 
  collapse = " + ")

inla_form <- as.formula(
  sprintf("y ~ -1 + policy + f(spatial.poli, model = spde.spatial) + %s + %s + 
      Greenness + BuiltUp + 
      f(dow, model = 'iid', group = id_dow, constr = F,
        extraconstr = list(A = Ac, e = 0))",
    fixed_part, spde_part)
)

#-------------------------------
# Models for each components with the remaining of SI 
#-------------------------------

# Initialize result list
pollist <- vector("list", ncomp); names(pollist) <- oxcgrt_policies
policy_reslist <- rep(list(pollist), np); names(policy_reslist) <- pollutants

# initialize results tab
score_tab <- cbind(expand.grid(policy = oxcgrt_policies, pollutant = pollutants),
  matrix(NA, ncomp * np, 3,
    dimnames = list(NULL, 
      c("policy_coef", "policy_low", "policy_high")))
)

# Loop on pollutant and on policy components
for (p in seq_len(np)){
  cat("p = ", p, "\n")
  
  # Choose outcome
  y <- datatab[,sprintf("Diff%s", pollutants[p])]
  
  # Loop on components
  for (i in seq_len(ncomp)){
    
    cat("i = ", i, "\n")
    
    #----- Create variables
    # Select the policy
    compVars <- grep(oxcgrt_policies[i], names(datatab))
    policy <- datatab[,compVars[1]]
    
    # Remove .5 according to the flag (general or targeted)
    if(length(compVars) > 1){
      policy <- policy - .5 * (1 - datatab[,compVars[2]])
      policy[is.na(policy)] <- 0
    }
    
    # Standardize the policy in [0;1]
    policy <- policy / max(policy, na.rm = T)
    
    # Remove component from SI and standardize SI
    siminus <- datatab$StringencyIndex - 100 / ncomp * policy
    siminus <- siminus / (100 * (ncomp - 1) / ncomp)
    
    # Spline of SI
    SIspline <- ns(siminus, df = sidf)
    colnames(SIspline) <- sprintf("ns%i", 1:sidf)
    
    #----- Create spatial variables
    # Create basis A matrix
    policy_A <- inla.spde.make.A(mesh, weights = policy,
      loc = data.matrix(datatab[,c("cities_lon", "cities_lat")]))
    spline_A <- apply(SIspline, 2, function(x){
      inla.spde.make.A(mesh, 
        loc = data.matrix(datatab[,c("cities_lon", "cities_lat")]),
        weights = x)
    })
    
    # Create stack object
    stack_obj <- inla.stack(data = list(y = y),
      A = c(policy_A, spline_A, list(1)),
      effects = c(list(policy_index), spline_index, 
        list(cbind(SIspline, policy, 
          datatab[,c("dow", "BuiltUp", "Greenness", "id_dow", "id_pol",
            sprintf("id_ns%i", 1:sidf))]))),
      tag = "estim")
    
    #----- Fit model with INLA
    policy_reslist[[p]][[i]] <- inla(inla_form,
      data = inla.stack.data(stack_obj), 
      control.predictor = list(A = inla.stack.A(stack_obj), compute = T)
    )
    
    #----- Extract fixed effect
    ind <- ncomp * (p - 1) + i
    score_tab[ind, c(3:5)] <- policy_reslist[[p]][[i]]$summary.fixed[1, c(1, 3, 5)]
  }
}
