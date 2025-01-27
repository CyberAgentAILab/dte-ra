##------------------------------------------------------------------------------
## Data generating process
##------------------------------------------------------------------------------
dgp = function(dgp_label, n, rho) {
  
  # Generate common variables
  D = rbinom(n, 1, rho)
  x1 = runif(n, min = 0.5, max = 1.5)
  x2 = rnorm(n, mean = 0, sd = 1)
  
  # Initialize y
  Y = NULL
  
  # Generate specific DGP
  if (dgp_label == 1) {
    u = rnorm(n, mean = 0, sd = 1)
    Y = x1 + x2 + (x1 + x2) * D + abs(x1 + x2) * u
  }
  
  else if (dgp_label==2 ) {
    e = rchisq(n, df = 3)
    Y = x1 + x2 + (x1 + x2) * D + abs(x1 + x2) * e
  }
  
  else if (dgp_label==3) {
    mu = exp(D + x1 + 0.5 * x2)
    Y = rpois(n, lambda = mu)
  } 
  
  else if (dgp_label==4) {
    xb = exp(D + x1 + 0.5 * x2)
    Y = rnbinom(n, size=5, mu=xb)
  } 
  
  # Return dataframe
  return(data.frame(cbind(Y, D, x1, x2)))
}


##------------------------------------------------------------------------------
## Computing distribution functions
##------------------------------------------------------------------------------
cdf.adj = function(d.treat, vec.y, vec.d, mat.x, vec.loc, model, num_poly){
  ##--------------
  ## setup   
  ##--------------
  n.loc   = length(vec.loc)    ## # of locations
  ind.pte = seq(1,length(vec.loc), by = h.pte)
  n.pte   = length(ind.pte) 
  col.sub = which(vec.d == d.treat)
  sub.y   = vec.y[col.sub]    ## y - subsample
  sub.x   = mat.x[col.sub,]   ## x - subsample
  
  ##--------------------------------
  ## empirical distribution function
  ##--------------------------------
  mat.d.y = 1 * outer(sub.y, vec.loc, "<=")  ## n x n.loc
  vec.cdf = as.matrix( colMeans(mat.d.y) )   ## n.loc x 1  
  vec.sum = as.matrix( colSums(mat.d.y) ) 
  
  ##----------------
  ## pdf estimation     
  ##----------------
  temp.cdf = vec.cdf[ind.pte]
  vec.pdf  = temp.cdf - c(0, temp.cdf[1:(n.pte-1)])    ## initial prob + difference 
  
  ##--------------
  ## variance      
  ##--------------
  vec.cdf.var = vec.cdf * (1 - vec.cdf)
  vec.pdf.var = vec.pdf * (1 - vec.pdf)
  
  ##-------------------------------------
  ## conditional distribution estimation
  ##-------------------------------------
  vec.cdf.ra = matrix(0, n.loc, 1)
  mu.sub = matrix(NA, length(sub.y), n.loc) # Predicted means in sub sample (treatment or control)
  mu.all = matrix(NA, length(vec.y), n.loc) # Predicted means in full sample
  
  model.fits = list() # To store model fits
  
  for(j in 1:n.loc){
    y = mat.d.y[,j] ## jth column - outcome
    
    if (model=="logistic_regression"){
      logit = fastglm(x=cbind(1, sub.x),
                      y= y,
                      family = binomial(link = "logit"))
      mu.sub[,j] =  predict(logit, cbind(1,sub.x), type="response")
      mu.all[,j] =  predict(logit, cbind(1, mat.x), type="response")
      model.fits[[j]] = logit
    }
    
    
    else if (model=="logistic_regression_poly"){
      # Logistic regression with polynomial transformations
      logit = fastglm(x=poly(sub.x, degree=num_poly, raw=TRUE),
                      y= y,
                      family = binomial(link = "logit"))
      mu.sub[,j] =  predict(logit, poly(sub.x, degree=num_poly, raw=TRUE), type="response")
      mu.all[,j] =  predict(logit, poly(mat.x, degree=num_poly, raw=TRUE), type="response")
      model.fits[[j]] = logit
    }
    
    else if (model=="ols"){
      data_train = as.data.frame(cbind(y, sub.x))
      colnames(data_train) = c("y", colnames(sub.x))
      new_x_sub = as.data.frame(sub.x)
      new_x_all = as.data.frame(mat.x)
      ols = lm(y~., data=data_train)
      mu.sub[,j] =  predict(ols, new_x_sub)
      mu.all[,j] =  predict(ols, new_x_all)
      model.fits[[j]] = ols
    }
    
    else if (model=="ols_poly"){
      # Linear regression with polynomial transformations
      data_train = as.data.frame(cbind(y, sub.x))
      ols = lm(y ~ poly(x1, x2, degree= num_poly, raw=TRUE) , data=data_train)
      mu.sub[,j] =  predict(ols, data_train)
      mu.all[,j] =  predict(ols, as.data.frame(mat.x))
      model.fits[[j]] = ols
    }
  }
  
  
  ##---------------------------------
  ## regression adjustment for CDF
  ##---------------------------------
  cdf.adjustment = colMeans(mu.all) - colMeans(mu.sub)
  vec.cdf.ra = vec.cdf + cdf.adjustment ## n.loc x 1
  
  ##---------------
  ## pdf estimation     
  ##---------------
  temp.cdf.ra = vec.cdf.ra[ind.pte]
  vec.pdf.ra  = temp.cdf.ra - c(0, temp.cdf.ra[1:(n.pte-1)])    ## initial prob + difference 
  
  ##--------------
  ## variance      
  ##--------------
  vec.cdf.var.ra = vec.cdf.ra * (1 - vec.cdf.ra)
  vec.pdf.var.ra = vec.pdf.ra * (1 - vec.pdf.ra)
  
  ## return 
  return(list(mu.all = mu.all,
              cdf = vec.cdf, 
              cdf.ra = vec.cdf.ra,
              cdf.adjustment = cdf.adjustment,
              cdf.var = vec.cdf.var,
              cdf.var.ra = vec.cdf.var.ra,
              pdf = vec.pdf,
              pdf.ra = vec.pdf.ra,
              pdf.var = vec.pdf.var,
              pdf.var.ra = vec.pdf.var.ra,
              num_obs = length(sub.y),
              model.fits = model.fits))
}


##------------------------------------------------------------------------------
## Regression-adjusted DTE and PTE estimation
##------------------------------------------------------------------------------
DTE.estimation = function(vec.y, vec.d, mat.x, vec.loc, model, num_poly=2, n_boot=500){
  
  num_obs = length(vec.y)
  
  ## locations   
  n.loc   = length(vec.loc)
  ind.pte = seq(1,length(vec.loc), by = h.pte)
  n.pte   = length(ind.pte) 
  
  ## containers 
  mat.cdf    = matrix(NA, n.loc, 2)
  mat.cdf.adjustment    = matrix(NA, n.loc, 2)
  mat.cdf.var = matrix(NA, n.loc, 2)
  mat.cdf.ra = matrix(NA, n.loc, 2)
  mat.cdf.var.ra = matrix(NA, n.loc, 2)
  mat.pdf    = matrix(NA, n.pte, 2)
  mat.pdf.var    = matrix(NA, n.pte, 2)
  mat.pdf.ra = matrix(NA, n.pte, 2)
  mat.pdf.var.ra = matrix(NA, n.pte, 2)
  mat.mu.all    = array(NA, c(2, num_obs, n.loc)) # Predictions for all observables
  treat_prob = matrix(NA, 1, 2 ) # Number of observations in each group
  
  res.cdf.all = list() # Store results for both groups
  
  ##------------------------------------------
  ## estimation for each group     
  ##------------------------------------------
  for(j in 1:2){  ## treatment/control group 
    
    ## data: subset: first, estimate "treatment" group and then "control" group     
    d.treat = 2 - j ## j=1 <-> 1:treatment & j=2 <-> 0:control  
    
    ## CDF estimation   
    res.cdf = cdf.adj(d.treat, vec.y, vec.d, mat.x, vec.loc, model, num_poly)
    res.cdf.all[[j]] = res.cdf
    
    mat.cdf[,j]     = res.cdf$cdf
    mat.cdf.var[,j] = res.cdf$cdf.var
    
    mat.cdf.ra[,j]  = res.cdf$cdf.ra
    mat.cdf.var.ra[,j] = res.cdf$cdf.var.ra
    mat.cdf.adjustment[,j] = res.cdf$cdf.adjustment
    
    mat.pdf[,j] = res.cdf$pdf
    mat.pdf.var[,j] = res.cdf$pdf.var
    
    mat.pdf.ra[,j] = res.cdf$pdf.ra
    mat.pdf.var.ra[,j] = res.cdf$pdf.var.ra
    
    mat.mu.all[j,,] = res.cdf$mu.all
    treat_prob[,j] = res.cdf$num_obs
    
  }
  
  ##------------------------------------------
  ## treatment effect estimation (DTE and PTE)      
  ##------------------------------------------
  vec.dte     = mat.cdf[,1]    - mat.cdf[,2]   
  vec.dte.ra  = mat.cdf.ra[,1] - mat.cdf.ra[,2]   
  vec.pte     = mat.pdf[,1]    - mat.pdf[,2]     
  vec.pte.ra  = mat.pdf.ra[,1] - mat.pdf.ra[,2] 
  
  
  ##------------------------------------  
  ## Analytic standard errors
  ##------------------------------------
  w1        = 1 / mean(vec.d)     
  w0        = 1/ (1 - mean(vec.d)) 
  
  vec.dte.var = (w1 * mat.cdf.var[,1]) + (w0 * mat.cdf.var[,2]) 
  vec.cv.dte = sqrt(vec.dte.var)/ sqrt(num_obs)
  
  vec.pte.var = (w1 * mat.pdf.var[,1]) + (w0 * mat.pdf.var[,2])  
  vec.cv.pte = sqrt(vec.pte.var)/ sqrt(num_obs)
  
  vec.dte.var.ra = (w1 * mat.cdf.var.ra[,1]) + (w0 * mat.cdf.var.ra[,2])  
  vec.cv.dte.ra = sqrt(vec.dte.var.ra) / sqrt(num_obs)
  
  vec.pte.var.ra = (w1 * mat.pdf.var.ra[,1]) + (w0 * mat.pdf.var.ra[,2]) 
  vec.cv.pte.ra = sqrt(vec.pte.var.ra) / sqrt(num_obs)
  
  # Predictions in treatment and control group
  mat.mu.1 = as.matrix(mat.mu.all[1,,])
  mat.mu.0 = as.matrix(mat.mu.all[2,,])
  
  # Number of observations in each group
  num_1 = treat_prob[,1]
  num_0 = treat_prob[,2]
  
  # Indicator that y is below threshold u
  mat.y.u = 1 * outer(vec.y, vec.loc, "<=")       ## n x n.loc
  mat.d = replicate(n.loc, vec.d)                 ## n x n.loc 
  mat.dte = t(matrix((rep(vec.dte, num_obs)), nrow=n.loc, ncol=num_obs))
  mat.dte.ra = t(matrix((rep(vec.dte.ra, num_obs)), nrow=n.loc, ncol=num_obs))
  
  # Sample analog of asymptotic variance
  # Adjusted DTE
  omega_moment.ra = (num_obs/num_1*(mat.d*(mat.y.u-mat.mu.1)) + mat.mu.1 - num_obs/num_0*((1-mat.d)*(mat.y.u-mat.mu.0)) - mat.mu.0 - mat.dte.ra)^2  ## n x n.loc
  omega.ra = colMeans(omega_moment.ra) ## 1 x n.loc
  ## Standard errors 
  vec.cv.dte.ra  = sqrt(omega.ra/num_obs)
  
  ##------------------------------------------
  ## bootstrap standard errors 
  ##------------------------------------------
  dte.boot = matrix(0, n_boot, n.loc)
  dte.ra.boot = matrix(0, n_boot, n.loc)
  
  for (b in 1:n_boot) {
    ## Resample indices with replacement
    boot.indices = sample(1:num_obs, num_obs, replace = TRUE)
    boot.y = vec.y[boot.indices]
    boot.d = vec.d[boot.indices]
    boot.x = mat.x[boot.indices, ]
    
    ## containers 
    mat.cdf.boot    = matrix(NA, n.loc, 2)
    mat.cdf.ra.boot    = matrix(NA, n.loc, 2)
    
    ## Recompute adjusted CDF using original model fits
    for (d in 1:2) {
      d.treat = 2 - d
      col.sub = which(boot.d == d.treat)
      boot.sub.x   = boot.x[col.sub,]   ## x - subsample
      boot.sub.y   = boot.y[col.sub]    ## y - subsample
      
      ## Bootstrapped CDF estimation   
      boot.cdf = cdf.adj(d.treat, boot.y, boot.d, boot.x, vec.loc, model, num_poly)
      mat.cdf.boot[, d] = boot.cdf$cdf
      
      ## Bootstrap adjustment
      res.original = res.cdf.all[[d]] # Original model fits
      mu.sub = matrix(NA, length(boot.sub.y), n.loc) # Predicted means in sub sample (treatment or control)
      mu.all = matrix(NA, length(boot.y), n.loc) # Predicted means in full sample  
      
      for(j in 1:n.loc){
        
        ## Generate predictions for bootstrap data using original model fits
        if (model == "logistic_regression") {
          mu.sub[,j] =  predict(res.original$model.fits[[j]], cbind(1, boot.sub.x), type="response")
          mu.all[,j] =  predict(res.original$model.fits[[j]], cbind(1, boot.x), type="response")
          
        } else if (model == "logistic_regression_poly") {
          mu.sub[,j] =  predict(res.original$model.fits[[j]], poly(boot.sub.x, degree=num_poly, raw=TRUE), type="response")
          mu.all[,j] =  predict(res.original$model.fits[[j]], poly(boot.x, degree=num_poly, raw=TRUE), type="response")
          
        } else if (model == "ols") {
          mu.sub[,j] =  predict(res.original$model.fits[[j]], as.data.frame(boot.sub.x))
          mu.all[,j] =  predict(res.original$model.fits[[j]], as.data.frame(boot.x))
          
        } else if (model == "ols_poly") {
          mu.sub[,j] =  predict(res.original$model.fits[[j]], as.data.frame(boot.sub.x))
          mu.all[,j] =  predict(res.original$model.fits[[j]], as.data.frame(boot.x))
        }
      }
      
      ## Recompute adjusted CDF for bootstrap sample
      mat.cdf.ra.boot[, d] = boot.cdf$cdf + colMeans(mu.all) - colMeans(mu.sub)
      
    }
    
    ## Compute bootstrap DTE (adjusted)
    dte.boot[b, ] = mat.cdf.boot[,1] - mat.cdf.boot[,2]
    dte.ra.boot[b, ] = mat.cdf.ra.boot[, 1] - mat.cdf.ra.boot[, 2]
  }
  
  
  ## Standard Errors from Bootstrap (SD)
  dte.boot.se = apply(dte.boot, 2, sd)
  dte.boot.ra.se = apply(dte.ra.boot, 2, sd)
  
  # Standard Errors from Bootstrap (IQR)
  dte.boot.robust.se = apply(dte.boot, 2, IQR)/(qnorm(0.75)- qnorm(0.25))
  dte.boot.ra.robust.se = apply(dte.ra.boot, 2, IQR)/(qnorm(0.75)- qnorm(0.25))
  
  
  ##---------------------  
  ## stack up results 
  ##---------------------
  est.dte    = data.frame( cbind(vec.loc, mat.cdf, vec.dte, vec.cv.dte, dte.boot.se, dte.boot.robust.se) )
  est.dte.ra = data.frame( cbind(vec.loc, mat.cdf.ra, vec.dte.ra, vec.cv.dte.ra, dte.boot.ra.se, dte.boot.ra.robust.se) )
  est.pte    = data.frame( cbind(vec.loc[ind.pte[1:n.pte]], mat.pdf,    vec.pte,  vec.cv.pte) )
  est.pte.ra = data.frame( cbind(vec.loc[ind.pte[1:n.pte]], mat.pdf.ra, vec.pte.ra,  vec.cv.pte.ra) )
  
  
  colnames(est.dte)    = c("vec.loc", "cdf1", "cdf0", "dte","se", "boot.se", "boot.robust.se")
  colnames(est.dte.ra) = c("vec.loc", "cdf1", "cdf0", "dte", "se", "boot.se", "boot.robust.se")
  colnames(est.pte)    = c("vec.loc", "pdf1", "pdf0", "pte", "se")
  colnames(est.pte.ra) = c("vec.loc", "pdf1", "pdf0", "pte", "se")
  
  
  
  
  return( list(dte     = est.dte, 
               pte     = est.pte,
               dte.ra  = est.dte.ra, 
               pte.ra  = est.pte.ra))
  
}


#------------------------------------------------------------------------------
## Run simulations for DTE
##------------------------------------------------------------------------------
run_simulation_dte = function(dgp_label, sim, model1, model2, num_poly=2){
  ## DGP 
  eval(parse(text=paste0("df=dgp(dgp_label, n, rho)")))
  
  ## data 
  vec.y = df$Y
  vec.d = df$D
  mat.x = as.matrix(df[, 3:ncol(df)])
  num_obs = n ## sample size (for whole dataset)
  
  ## model
  res.ols = DTE.estimation(vec.y, vec.d, mat.x, vec.loc,  model1, num_poly)
  res.logit = DTE.estimation(vec.y, vec.d, mat.x, vec.loc, model2, num_poly)
  
  results = list(res.ols = res.ols, res.logit=res.logit)
  
  ## coverage probability & average length of CI
  # Analytic standard errors
  res0.coverage.temp = as.numeric(res.ols$dte$dte -1.96*res.ols$dte$se < dte.true & 
                                    dte.true < res.ols$dte$dte +1.96*res.ols$dte$se)
  res0.length.temp = abs(2*1.96*res.ols$dte$se)
  
  ## OLS adjusted
  res1.coverage.temp = as.numeric(res.ols$dte.ra$dte -1.96*res.ols$dte.ra$se < dte.true & 
                                    dte.true < res.ols$dte.ra$dte +1.96*res.ols$dte.ra$se)
  res1.length.temp = abs(2*1.96*res.ols$dte.ra$se)
  
  ## Logit adjusted
  res2.coverage.temp = as.numeric(res.logit$dte.ra$dte -1.96*res.logit$dte.ra$se < dte.true & 
                                    dte.true < res.logit$dte.ra$dte +1.96*res.logit$dte.ra$se)
  res2.length.temp = abs(2*1.96*res.logit$dte.ra$se)
  
  coverage.temp = cbind(res0.coverage.temp, res1.coverage.temp, res2.coverage.temp)
  length.temp = cbind(res0.length.temp, res1.length.temp, res2.length.temp)
  
  # Bootstrap standard deviation
  res0.boot.coverage.temp = as.numeric(res.ols$dte$dte -1.96*res.ols$dte$boot.se < dte.true & 
                                         dte.true < res.ols$dte$dte + 1.96*res.ols$dte$boot.se)
  res0.boot.length.temp = abs(2*1.96*res.ols$dte$boot.se)
  
  res1.boot.coverage.temp = as.numeric(res.ols$dte.ra$dte -1.96*res.ols$dte.ra$boot.se < dte.true &
                                         dte.true < res.ols$dte.ra$dte + 1.96*res.ols$dte.ra$boot.se)
  res1.boot.length.temp = abs(2*1.96*res.ols$dte.ra$boot.se)
  
  res2.boot.coverage.temp = as.numeric(res.logit$dte.ra$dte -1.96*res.logit$dte.ra$boot.se < dte.true &
                                         dte.true < res.logit$dte.ra$dte + 1.96*res.logit$dte.ra$boot.se)
  res2.boot.length.temp = abs(2*1.96*res.logit$dte.ra$boot.se)
  
  boot.coverage.temp = cbind(res0.boot.coverage.temp, res1.boot.coverage.temp, res2.boot.coverage.temp)
  boot.length.temp = cbind(res0.boot.length.temp, res1.boot.length.temp, res2.boot.length.temp)
  
  # Bootstrap IQR
  res0.boot.robust.coverage.temp = as.numeric(res.ols$dte$dte -1.96*res.ols$dte$boot.robust.se < dte.true &
                                                dte.true < res.ols$dte$dte + 1.96*res.ols$dte$boot.robust.se)
  res0.boot.robust.length.temp = abs(2*1.96*res.ols$dte$boot.robust.se)
  
  res1.boot.robust.coverage.temp = as.numeric(res.ols$dte.ra$dte -1.96*res.ols$dte.ra$boot.robust.se < dte.true &
                                                dte.true < res.ols$dte.ra$dte + 1.96*res.ols$dte.ra$boot.robust.se)
  res1.boot.robust.length.temp = abs(2*1.96*res.ols$dte.ra$boot.robust.se)
  
  res2.boot.robust.coverage.temp = as.numeric(res.logit$dte.ra$dte -1.96*res.logit$dte.ra$boot.robust.se < dte.true &
                                                dte.true < res.logit$dte.ra$dte + 1.96*res.logit$dte.ra$boot.robust.se)
  res2.boot.robust.length.temp = abs(2*1.96*res.logit$dte.ra$boot.robust.se)
  
  boot.robust.coverage.temp = cbind(res0.boot.robust.coverage.temp, res1.boot.robust.coverage.temp, res2.boot.robust.coverage.temp)
  boot.robust.length.temp = cbind(res0.boot.robust.length.temp, res1.boot.robust.length.temp, res2.boot.robust.length.temp)
  
  
  
  results = list(res.ols = res.ols, res.logit=res.logit, 
                 coverage.temp=coverage.temp, length.temp=length.temp,
                 boot.coverage.temp=boot.coverage.temp, boot.length.temp=boot.length.temp,
                 boot.robust.coverage.temp=boot.robust.coverage.temp, boot.robust.length.temp=boot.robust.length.temp
                 
  )
  
  # saveRDS(results, file=paste0("../result/dte_dgp", dgp.n, "_rho", rho, "_n", n,  "_s", sim, ".rds"))
  
  ## return
  return (list(res.ols = res.ols, res.logit=res.logit, 
               coverage.temp=coverage.temp, length.temp=length.temp,
               boot.coverage.temp=boot.coverage.temp, boot.length.temp=boot.length.temp,
               boot.robust.coverage.temp=boot.robust.coverage.temp, boot.robust.length.temp=boot.robust.length.temp
  ))
}








