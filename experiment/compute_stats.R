rm(list = ls())

##------------------------------------------------------------------------------
## Load libraries and source files
##------------------------------------------------------------------------------
library(dplyr)
library(foreign)
library(ggpubr) 

##------------------------------------------------------------------------------
## Main simulations
##------------------------------------------------------------------------------
vec.rho  = c(0.3, 0.5)
vec.n = c(300, 500, 1000) 
n.sim = 1000

for(dgp.n in 1:4){
  
  if(dgp.n == 1 | dgp.n == 2){  ## continuous outcome 
    vec.loc  = seq(0.1, 0.9, 0.1)  
  } else{ ## discrete outcome 
    vec.loc  = seq(1, 5, by=1)
  }
  
for(j.rho in 1:length(vec.rho)){
  
  rho = vec.rho[j.rho]
  
  # Load true DTE
  dte.true = readRDS(paste0("../result/dgp", dgp.n, "_rho", rho, "_dte.true.rds"))
  
  for(j in 1:length(vec.n)) { ## loop: sample size 
  
  n = vec.n[j]  ## sample size 
  
  results = readRDS(paste0("../result/dgp", dgp.n, "_rho", rho, "_n", n, "_s", n.sim, ".rds"))
  
  # RMSE
  rmse0 = sqrt(Reduce('+', lapply(results, function(mat)(mat$res.ols$dte$dte-dte.true)^2))/n.sim)
  rmse1 = sqrt(Reduce('+', lapply(results, function(mat)(mat$res.ols$dte.ra$dte-dte.true)^2))/n.sim)
  rmse2 = sqrt(Reduce('+', lapply(results, function(mat)(mat$res.logit$dte.ra$dte-dte.true)^2))/n.sim)
  
  rmse  = cbind(n, vec.loc, rmse0, rmse1, rmse2)
  colnames(rmse) = c( "n","location","Simple","OLS", "Logit")
  
  # Bias
  bias0 = Reduce('+', lapply(results, function(mat)(mat$res.ols$dte$dte-dte.true)))/n.sim
  bias1 = Reduce('+', lapply(results, function(mat)(mat$res.ols$dte.ra$dte-dte.true)))/n.sim
  bias2 = Reduce('+', lapply(results, function(mat)(mat$res.logit$dte.ra$dte-dte.true)))/n.sim
  
  bias = cbind(n, vec.loc, bias0, bias1, bias2)
  colnames(bias) = c( "n","location","Simple","OLS", "Logit")
  
  # RMSE ratio
  ratio = cbind(n, vec.loc, 100*(1-rmse1/rmse0), 100*(1-rmse2/rmse0))
  colnames(ratio) = c( "n","location","OLS", "Logit")
  
  # Coverage probability
  coverage = Reduce('+', lapply(results, function(mat)(mat$coverage.temp)))/n.sim
  coverage = cbind(n, vec.loc, coverage)
  colnames(coverage) = c( "n","location","Simple","OLS", "Logit")
  
  # Average CI length
  average.length = Reduce('+', lapply(results, function(mat)(mat$length.temp)))/n.sim
  average.length = cbind(n, vec.loc, average.length)
  colnames(average.length) = c( "n","location","Simple","OLS", "Logit")
  
  # Bootstrap Coverage probability
  boot.coverage = Reduce('+', lapply(results, function(mat)(mat$boot.coverage.temp)))/n.sim
  boot.coverage = cbind(n, vec.loc, boot.coverage)
  colnames(boot.coverage) = c( "n","location","Simple","OLS", "Logit")
  
  # Average CI length
  boot.average.length = Reduce('+', lapply(results, function(mat)(mat$boot.length.temp)))/n.sim
  boot.average.length = cbind(n, vec.loc, boot.average.length)
  colnames(boot.average.length) = c( "n","location","Simple","OLS", "Logit")
  
  # Coverage probability
  boot.robust.coverage = Reduce('+', lapply(results, function(mat)(mat$boot.robust.coverage.temp)))/n.sim
  boot.robust.coverage = cbind(n, vec.loc, boot.robust.coverage)
  colnames(boot.robust.coverage) = c( "n","location","Simple","OLS", "Logit")
  
  # Average CI length
  boot.robust.average.length = Reduce('+', lapply(results, function(mat)(mat$boot.robust.length.temp)))/n.sim
  boot.robust.average.length = cbind(n, vec.loc, boot.robust.average.length)
  colnames(boot.robust.average.length) = c( "n","location","Simple","OLS", "Logit")
  
  
  # Save results as csv files
  v.info = paste0("dgp", dgp.n, "_rho", rho, "_n", n)
  write.csv(rmse, file=paste0("../result/", v.info, "_rmse.csv"), row.names=FALSE)
  write.csv(ratio, file=paste0("../result/", v.info, "_ratio.csv"), row.names=FALSE)
  write.csv(bias, file=paste0("../result/", v.info, "_bias.csv"), row.names=FALSE )
  write.csv(coverage, file=paste0("../result/", v.info, "_coverage.csv"), row.names=FALSE )
  write.csv(average.length, file=paste0("../result/", v.info, "_averagelength.csv"), row.names=FALSE )
  write.csv(boot.coverage, file=paste0("../result/", v.info, "_boot_coverage.csv"), row.names=FALSE )
  write.csv(boot.average.length, file=paste0("../result/", v.info, "_boot_averagelength.csv"), row.names=FALSE )
  write.csv(boot.robust.coverage, file=paste0("../result/", v.info, "_boot_robust_coverage.csv"), row.names=FALSE )
  write.csv(boot.robust.average.length, file=paste0("../result/", v.info, "_boot_robust_averagelength.csv"), row.names=FALSE )
  write.csv(dte.true, file = paste0("../result/", v.info, "_dte.true.csv"), row.names=FALSE)
}

}
}

##------------------------------------------------------------------------------
## Appendix: simulations with covariate transformations
##------------------------------------------------------------------------------
vec.poly = c(2, 3) # degrees of polynomials to consider

for (j in 1:length(vec.poly)){
  num_poly = vec.poly[j]
  
  dgp.n = 1
  rho = 0.5
  
  # Load true DTE
  dte.true = readRDS(paste0("../result/dgp", dgp.n, "_rho", rho, "_poly", num_poly, "_dte.true.rds"))
  
  for(j in 1:length(vec.n)) { ## loop: sample size 
    
    n = vec.n[j]  ## sample size 
    
    results = readRDS(paste0("../result/dgp", dgp.n, "_rho", rho, "_poly", num_poly, "_n", n, "_s", n.sim, ".rds"))
    
    # RMSE
    rmse0 = sqrt(Reduce('+', lapply(results, function(mat)(mat$res.ols$dte$dte-dte.true)^2))/n.sim)
    rmse1 = sqrt(Reduce('+', lapply(results, function(mat)(mat$res.ols$dte.ra$dte-dte.true)^2))/n.sim)
    rmse2 = sqrt(Reduce('+', lapply(results, function(mat)(mat$res.logit$dte.ra$dte-dte.true)^2))/n.sim)
    
    rmse  = cbind(n, vec.loc, rmse0, rmse1, rmse2)
    colnames(rmse) = c( "n","location","Simple","OLS", "Logit")
    
    # Bias
    bias0 = Reduce('+', lapply(results, function(mat)(mat$res.ols$dte$dte-dte.true)))/n.sim
    bias1 = Reduce('+', lapply(results, function(mat)(mat$res.ols$dte.ra$dte-dte.true)))/n.sim
    bias2 = Reduce('+', lapply(results, function(mat)(mat$res.logit$dte.ra$dte-dte.true)))/n.sim
    
    bias = cbind(n, vec.loc, bias0, bias1, bias2)
    colnames(bias) = c( "n","location","Simple","OLS", "Logit")
    
    # RMSE ratio
    ratio = cbind(n, vec.loc, 100*(1-rmse1/rmse0), 100*(1-rmse2/rmse0))
    colnames(ratio) = c( "n","location","OLS", "Logit")
    
    # Coverage probability
    coverage = Reduce('+', lapply(results, function(mat)(mat$coverage.temp)))/n.sim
    coverage = cbind(n, vec.loc, coverage)
    colnames(coverage) = c( "n","location","Simple","OLS", "Logit")
    
    # Average CI length
    average.length = Reduce('+', lapply(results, function(mat)(mat$length.temp)))/n.sim
    average.length = cbind(n, vec.loc, average.length)
    colnames(average.length) = c( "n","location","Simple","OLS", "Logit")
    
    # Bootstrap Coverage probability
    boot.coverage = Reduce('+', lapply(results, function(mat)(mat$boot.coverage.temp)))/n.sim
    boot.coverage = cbind(n, vec.loc, boot.coverage)
    colnames(boot.coverage) = c( "n","location","Simple","OLS", "Logit")
    
    # Average CI length
    boot.average.length = Reduce('+', lapply(results, function(mat)(mat$boot.length.temp)))/n.sim
    boot.average.length = cbind(n, vec.loc, boot.average.length)
    colnames(boot.average.length) = c( "n","location","Simple","OLS", "Logit")
    
    # Coverage probability
    boot.robust.coverage = Reduce('+', lapply(results, function(mat)(mat$boot.robust.coverage.temp)))/n.sim
    boot.robust.coverage = cbind(n, vec.loc, boot.robust.coverage)
    colnames(boot.robust.coverage) = c( "n","location","Simple","OLS", "Logit")
    
    # Average CI length
    boot.robust.average.length = Reduce('+', lapply(results, function(mat)(mat$boot.robust.length.temp)))/n.sim
    boot.robust.average.length = cbind(n, vec.loc, boot.robust.average.length)
    colnames(boot.robust.average.length) = c( "n","location","Simple","OLS", "Logit")
    
    
    # Save results as csv files
    v.info = paste0("dgp", dgp.n, "_rho", rho, "_poly", num_poly, "_n", n)
    write.csv(rmse, file=paste0("../result/", v.info, "_rmse.csv"), row.names=FALSE)
    write.csv(ratio, file=paste0("../result/", v.info, "_ratio.csv"), row.names=FALSE)
    write.csv(bias, file=paste0("../result/", v.info, "_bias.csv"), row.names=FALSE )
    write.csv(coverage, file=paste0("../result/", v.info, "_coverage.csv"), row.names=FALSE )
    write.csv(average.length, file=paste0("../result/", v.info, "_averagelength.csv"), row.names=FALSE )
    write.csv(boot.coverage, file=paste0("../result/", v.info, "_boot_coverage.csv"), row.names=FALSE )
    write.csv(boot.average.length, file=paste0("../result/", v.info, "_boot_averagelength.csv"), row.names=FALSE )
    write.csv(boot.robust.coverage, file=paste0("../result/", v.info, "_boot_robust_coverage.csv"), row.names=FALSE )
    write.csv(boot.robust.average.length, file=paste0("../result/", v.info, "_boot_robust_averagelength.csv"), row.names=FALSE )
    write.csv(dte.true, file = paste0("../result/", v.info, "_dte.true.csv"), row.names=FALSE)
  }
}
  
  
  



