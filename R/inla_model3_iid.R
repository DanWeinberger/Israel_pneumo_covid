inla_model2 <- function(ds,outcome.var,prec.prior1=1,plot.var='CAAP'){
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
  
  
  covars=c('influenza.a','influenza.b','RSV','hMPV','respiratory.adenovirus','parainfluenza')
  
  #eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth*(t.scale+ sin12+cos12  +", paste(covars, collapse='+') ,")"))
  
  #eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth + t.scale + sin12+cos12  +", paste(covars, collapse='+') ))
 
  #eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth*(t.scale+ sin12+cos12  + RSV + hMPV  + influenza.a + influenza.b +respiratory.adenovirus +parainfluenza" ,")"))
 
  eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth + t.scale+ sin12+cos12 +   RSV + hMPV  + influenza.a + influenza.b +respiratory.adenovirus +parainfluenza" ))
  
 eqn.Za <-as.formula("~ -1+ agec_eth:t.scale + agec_eth:sin12 + agec_eth:cos12")
 
 eqn.Zb <-as.formula("~ -1 + agec_eth:RSV + agec_eth:hMPV +agec_eth:influenza.a +agec_eth:influenza.b + agec_eth:respiratory.adenovirus +
                       agec_eth:parainfluenza")
 
   
  Za <- model.matrix(eqn.Za, data=ds)
  
  Zb <- model.matrix( eqn.Zb, data=ds)
  
  X <- model.matrix(eqn.x1, data=ds)
  
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
  
  
  X.noflu <- model.matrix(eqn.x1, data=ds.noflu)
  X.norsv <- model.matrix(eqn.x1, data=ds.norsv)
  X.nohmpv <- model.matrix(eqn.x1, data=ds.nohmpv)
  X.noadeno <- model.matrix(eqn.x1, data=ds.noadeno)
  X.noparaflu <- model.matrix(eqn.x1, data=ds.noparaflu)
  X.novirus <- model.matrix(eqn.x1, data=ds.novirus)
  
  Zb.noflu <- model.matrix(eqn.Zb, data=ds.noflu)
  Zb.norsv <- model.matrix(eqn.Zb, data=ds.norsv)
  Zb.nohmpv <- model.matrix(eqn.Zb, data=ds.nohmpv)
  Zb.noadeno <- model.matrix(eqn.Zb, data=ds.noadeno)
  Zb.noparaflu <- model.matrix(eqn.Zb, data=ds.noparaflu)
  Zb.novirus <- model.matrix(eqn.Zb, data=ds.novirus)


  denom = ds$pop/1000

  #NOTE priors on precision are for LOG precision. so prec.intercept=0 is exp(0)=1
  prior.fixed <- list(mean.intercept= 0, prec.intercept = log(1),
                      mean = 0, prec =  log(1))

  
   y <- ds[,outcome.var]/denom

   ds$agec_eth2 <- ds$agec_eth
   ds$agec_eth3 <- ds$agec_eth
   ds$agec_eth4 <- ds$agec_eth
   ds$agec_eth5 <- ds$agec_eth
   ds$agec_eth6 <- ds$agec_eth
   ds$agec_eth7 <- ds$agec_eth
   ds$agec_eth8 <- ds$agec_eth
   ds$agec_eth9 <- ds$agec_eth
   
   
   
      formula <- y ~ -1  + X + f(agec_eth, sin12, model = "iid") +
     f(agec_eth2, cos12, model = "iid") +
     f(agec_eth3, t.scale, model = "iid") +
     f(agec_eth4, RSV, model = "iid") +
     f(agec_eth5, hMPV, model = "iid") +
     f(agec_eth6, influenza.a, model = "iid") +
     f(agec_eth7, influenza.b, model = "iid") +
     f(agec_eth8, parainfluenza, model = "iid") +
     f(agec_eth9, respiratory.adenovirus, model = "iid") 
     
   
   n <- nrow(ds)
   ds$y=y
   
  mod.inla2 <- inla(y ~ -1  + X + f(agec_eth, sin12, model = "iid") +
                      f(agec_eth2, cos12, model = "iid") +
                      f(agec_eth3, t.scale, model = "iid") +
                      f(agec_eth4, RSV, model = "iid") +
                      f(agec_eth5, hMPV, model = "iid") +
                      f(agec_eth6, influenza.a, model = "iid") +
                      f(agec_eth7, influenza.b, model = "iid") +
                      f(agec_eth8, parainfluenza, model = "iid") +
                      f(agec_eth9, respiratory.adenovirus, model = "iid"), data = ds, 
                    family='gaussian', 
                    control.predictor = list(compute=TRUE),
                    control.fixed = prior.fixed,
                    control.compute=list(config = TRUE, dic=T, waic=T))  
  
  
  waic = mod.inla2$waic$waic
  dic = mod.inla2$dic$dic
  
  all.ts <- sort(unique(ds$t))
  t.increment <- all.ts[length(all.ts)] - all.ts[length(all.ts)-1] 
  
  X.test <- list('all'=X, 'NoFlu'=X.noflu, 'NoRSV'=X.norsv, 'noHMPV'=X.nohmpv, 'NoVirus'=X.novirus)
  Zb.test <- list('all'=Zb, 'NoFlu'=Zb.noflu, 'NoRSV'=Zb.norsv, 'noHMPV'=Zb.nohmpv, 'NoVirus'=Zb.novirus)
  res1 <- mapply(FUN=gen_pred_interval_inla_ridge_ar1, X1=X.test,Zb=Zb.test,  MoreArgs=list(inla_obj=mod.inla2, covar.df=ds,Za=Za, outcome_name=plot.var,offset1= denom), SIMPLIFY=F)
  
  
  preds.inla2= c(res1, 'waic'=waic, 'dic'=dic)
  return(preds.inla2)
}