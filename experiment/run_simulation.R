rm(list = ls())

##------------------------------------------------------------------------------
## Load libraries and source files
##------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
require(gridExtra)
library(ggpubr)
library(foreign)
library(doParallel)
library(fastglm)

# Call source files
source( "functions.R" ) 

##------------------------------------------------------------------------------
## Setup
##------------------------------------------------------------------------------
# Set seed
set.seed(123)
num_cores = detectCores()-1
registerDoParallel(cores = num_cores) # Parallel backend

##------------------------------------------------------------------------------
## Simulation setup
##------------------------------------------------------------------------------
# Number of simulations
n.sim  = 1000
# Sample sizes
vec.n    = c(300, 500, 1000)   
# Treatment assignment probability
vec.rho  = c(0.3, 0.5)
# Gap length for PTE
h.pte   = 1  
# bootstrap replications
boot.size = 500

##------------------------------------------------------------------------------
## Main simulations 
##------------------------------------------------------------------------------
start_time = Sys.time()
for (dgp.n in 1:4){
  for(j.rho in 1:length(vec.rho)){
    
    rho = vec.rho[j.rho]
    
    # Approximate true distribution
    df.true = dgp(dgp.n, 1000000, rho)
    ymin = min(df.true$Y)
    ymax = max(df.true$Y)
    # Locations for DTE, quantiles 0.1, ..., 0.9
    ## location 
    if(dgp.n == 1 | dgp.n == 2){  ## continuous outcome 
      vec.loc  = quantile(df.true$Y, seq(0.1, 0.9, by=0.1) )  ## dgp1
    } else{ ## discrete outcome 
      vec.loc  = seq(1, 5, by=1)
    }
    
    #vec.loc  = quantile(df.true$y, seq(0.1, 0.9, by=0.1))    
    n.loc = length(vec.loc)
    ## Distributional treatment effect  (DTE)
    dte.true = matrix(NA, n.loc, 1)
    for(j in 1: n.loc ){
      v.loc   = vec.loc[j] ## location 
      dte.true[j]  = mean( (df.true$Y[which(df.true$D==1)] <= v.loc) ) -
        mean( (df.true$Y[which(df.true$D==0)] <= v.loc) ) 
      
    }
    # Save true DTE
    saveRDS(dte.true, file=paste0("../result/dgp", dgp.n, "_rho", rho, "_dte.true.rds"))
    
    
    for(j in 1:length(vec.n)) { ## loop: sample size 
      n = vec.n[j]  ## sample size 
      # Get results for each simulation run
      sim.output = foreach(i=1:n.sim) %dopar% run_simulation_dte(dgp.n, i, "ols", "logistic_regression")
      # Save the list using saveRDS
      print(paste("Save results with sample size", n))
      v.info = paste0("dgp", dgp.n, "_rho", rho, "_n", n, "_s", n.sim)
      saveRDS(sim.output, file=paste0("../result/", v.info, ".rds"))
    }
  }
}

end_time = Sys.time()
print(paste("Time spent:", end_time-start_time))

##------------------------------------------------------------------------------
## Appendix: simulations with covariate transformations
##------------------------------------------------------------------------------
start_time = Sys.time()
vec.poly = c(2, 3) # degrees of polynomials to consider

for (j in 1:length(vec.poly)){
  num_poly = vec.poly[j]
  
  dgp.n = 1
  rho = 0.5
  
  # Approximate true distribution
  df.true = dgp(dgp.n, 1000000, rho)
  ymin = min(df.true$Y)
  ymax = max(df.true$Y)
  # Locations for DTE, quantiles 0.1, ..., 0.9
  ## location 
  if(dgp.n == 1 | dgp.n == 2){  ## continuous outcome 
    vec.loc  = quantile(df.true$Y, seq(0.1, 0.9, by=0.1) )  ## dgp1
  } else{ ## discrete outcome 
    vec.loc  = seq(1, 5, by=1)
  }
  
  #vec.loc  = quantile(df.true$y, seq(0.1, 0.9, by=0.1))    
  n.loc = length(vec.loc)
  ## Distributional treatment effect  (DTE)
  dte.true = matrix(NA, n.loc, 1)
  for(j in 1: n.loc ){
    v.loc   = vec.loc[j] ## location 
    dte.true[j]  = mean( (df.true$Y[which(df.true$D==1)] <= v.loc) ) -
      mean( (df.true$Y[which(df.true$D==0)] <= v.loc) ) 
    
  }
  # Save true DTE
  saveRDS(dte.true, file=paste0("../result/dgp", dgp.n, "_rho", rho, "_poly", num_poly, "_dte.true.rds"))
  
  
  for(j in 1:length(vec.n)) { ## loop: sample size 
    n = vec.n[j]  ## sample size 
    # Get results for each simulation run
    sim.output = foreach(i=1:n.sim) %dopar% run_simulation_dte(dgp.n, i, "ols_poly", "logistic_regression_poly", num_poly)
    # Save the list using saveRDS
    print(paste("Save results with sample size", n))
    v.info = paste0("dgp", dgp.n, "_rho", rho, "_poly", num_poly, "_n", n, "_s", n.sim)
    saveRDS(sim.output, file=paste0("../result/", v.info, ".rds"))
  }
}
end_time = Sys.time()
print(paste("Time spent:", end_time-start_time))










