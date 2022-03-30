inla_model2_fixed <- function(ds,outcome.var,prec.prior1=1,plot.var='CAAP'){
  #http://julianfaraway.github.io/brinla/examples/ridge.html
  #ds=ag3
  
  ds$agec_eth <- as.factor( paste(ds$agec, ds$ethnicity, sep='_') )
  
  ds$year <- year(ds$date)
  ds$month <- month(ds$date)
  ds$t <- 12*(ds$year + ds$month/12 - 1/12 -2016)
  
  ds$t.scale <- ds$t/max(ds$t)

  ds$influenza.a <- ds$influenza.a/max(ds$influenza.a)
  ds$influenza.b <- ds$influenza.b/max(ds$influenza.a)
  ds$RSV <- ds$RSV/max(ds$RSV)
  ds$hMPV <- ds$hMPV/max(ds$hMPV)
  ds$respiratory.adenovirus <- ds$respiratory.adenovirus/max(ds$respiratory.adenovirus)
  ds$parainfluenza <- ds$parainfluenza/max(ds$parainfluenza)
  
  #covars=c('RSV','hMPV','influenza.a','influenza.b')
  covars=c('influenza.a','influenza.b','RSV','hMPV','respiratory.adenovirus','parainfluenza')
  
  #RSV, hMPV: DIC=311; WAIC=345
  #RSV, hMPV, fluA, fluB: DIC: 302; WAIC: 349
  # + 'respiratory.adenovirus','parainfluenza': DIC: 302, WAIC=348
  
  #DIC368, WAIC:399
  eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth*(t.scale+ sin12+cos12  +", paste(covars, collapse='+') ,")"))
  
  #379, WAIC:414
  #  eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth*(t.scale+ sin12+cos12)  + agec*(", paste(covars, collapse='+') ,")"))
  
  ds.noflu <- ds
  ds.noflu$influenza.a <- 0
  ds.noflu$influenza.b <- 0
  
  ds.norsv <- ds
  ds.norsv$RSV <- 0
  
  ds.nohmpv <- ds
  ds.nohmpv$hMPV <- 0
  
  ds.noadeno <- ds
  ds.noadeno$respiratory.adenovirus <- 0
  
  ds.noparaflu <- ds
  ds.noparaflu$parainfluenza <- 0
  
  ds.novirus <- ds 
  ds.novirus$influenza.a <- 0
  ds.novirus$influenza.b <- 0
  ds.novirus$RSV <- 0
  ds.novirus$respiratory.adenovirus <- 0
  ds.novirus$parainfluenza <- 0
  ds.novirus$hMPV <- 0
  
  X <- model.matrix(eqn.x1, data=ds)
  
  X.noflu <- model.matrix(eqn.x1, data=ds.noflu)
  X.norsv <- model.matrix(eqn.x1, data=ds.norsv)
  X.nohmpv <- model.matrix(eqn.x1, data=ds.nohmpv)
  X.noadeno <- model.matrix(eqn.x1, data=ds.noadeno)
  X.noparaflu <- model.matrix(eqn.x1, data=ds.noparaflu)
  X.novirus <- model.matrix(eqn.x1, data=ds.novirus)
  
  denom = ds$pop/1000

  #NOTE priors on precision are for LOG precision. so prec.intercept=0 is exp(0)=1
  prior.fixed <- list(mean.intercept= 0, prec.intercept = log(1),
                      mean = 0, prec =  log(1))

  
   y <- ds[,outcome.var]/denom
   mean.y <- mean(y, na.rm=T)
   sd.y <- sd(y, na.rm=T) 
   y.scale <- (y - mean.y)/sd.y

   formula <- y.scale ~ -1  + X 
   
   n <- nrow(ds)
   
  mod.inla2 <- inla(formula, data = list(y.scale=y.scale, 
                                         t=ds$t,
                                         X=X), 
                    family='gaussian', 
                    control.predictor = list(compute=TRUE),
                    control.fixed = prior.fixed,
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  
  
  waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  all.ts <- sort(unique(ds$t))
  t.increment <- all.ts[length(all.ts)] - all.ts[length(all.ts)-1] 
  
  X.test <- list('all'=X, 'NoFlu'=X.noflu, 'NoRSV'=X.norsv, 'noHMPV'=X.nohmpv, 'NoVirus'=X.novirus)

  res1 <- lapply(X.test, gen_pred_interval_inla_fixed, inla_obj=mod.inla2, covar.df=ds, outcome_name=plot.var,offset1= denom, mean.y=mean.y, sd.y=sd.y)
  
  
  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic)
  return(preds.inla2)
}