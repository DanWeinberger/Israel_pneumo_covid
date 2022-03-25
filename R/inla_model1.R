inla_model3 <- function(ds,outcome.var,prec.prior1=1){
  #http://julianfaraway.github.io/brinla/examples/ridge.html
  #ds=ag3
  
  ds$agec_eth <- as.factor( paste(ds$agec, ds$ethnicity, sep='_') )
  
  ds$t <- ds$year + ds$qtr/4 - 1/4 -2014
  
  ds$t <- ds$t / max(ds$t)
  
  
  eqn.x0<- as.formula(paste0(' ~ 1+ t +sin12 +cos12 +', paste(covars, collapse='+') )            )
  #form1.X<- as.formula(paste0(outcome.var,' ~ 1+ t.scale +sin12 +cos12 +', paste(covars, collapse='+'), "+f(t, model='ar1')+ offset(log_pop)")) 
  
  mod.mat1 <- model.matrix( eqn.x0 , data=ds)
  

  X <- model.matrix(eqn.x0, data=ds)
  # X <- as(mod.mat.X, "sparseMatrix")
  
  form.Za <- as.formula(paste0(" ~ -1+ agec_eth*(t+ sin12+cos12  +", paste(covars, collapse='+') ,")"))
  
  ds$agec <- as.factor(ds$agec)
  ds$ethnicity <- as.factor(ds$ethnicity)

    #using the : instead of * ensure main effect is not included, just the interaction. this is needed bc we have a fixed effect already
  Za <- model.matrix(~ -1 +
                       agec_eth:t + agec_eth:sin12 + agec_eth:cos12 +
                       agec_eth:RSV + agec_eth:hMPV +agec_eth:influenza.a + agec_eth:influenza.b +
                       agec_eth:respiratory.adenovirus + agec_eth:parainfluenza , data=ds)

  mod.mat.comb <- cbind(X, Za)

  n1 <- nrow(ds)
  
  y <- ds[,outcome.var]/ds$pop*100000
  formula <- y ~ -1  + X +  f(idx.Za, model="z", Z=Za) 
  
  mod.inla2 <- inla(formula, data = list(y=y, idx.Za = 1:n1, 
                                         t=ds$t,
                                         X=X), 
                    family='gaussian', 
                    control.predictor = list(compute=TRUE),
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  
  
  waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  all.ts <- sort(unique(ds$t))
  t.increment <- all.ts[length(all.ts)] - all.ts[length(all.ts)-1] 
  
  res1 <- gen_pred_interval_inla_ridge_ar1(inla_obj=mod.inla2, covar.df=ds,X=X,Za=Za,Zb=Zb, Zc=Zc, mod.mat=mod.mat.comb , source= unique(ds$source) ,log.offset1=ds$log_pop)
  
  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic)
  return(preds.inla2)
}