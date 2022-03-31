inla_model3 <- function(ds,outcome.var,prec.prior1=1,plot.var='CAAP'){
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
  

  #eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth*(t.scale+ sin12+cos12  +", paste(covars, collapse='+') ,")"))
  
  #eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth + t.scale + sin12+cos12  +", paste(covars, collapse='+') ))
 
  #eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth*(t.scale+ sin12+cos12  + RSV + hMPV  + influenza.a + influenza.b +respiratory.adenovirus +parainfluenza" ,")"))
 
  eqn.x1 <- as.formula(paste0(" ~ 1+ agec_eth + t.scale+ sin12+cos12 +" ,paste(covars, collapse='+')) ) 
  
 eqn.Za <-as.formula("~ -1+ agec_eth:t.scale + agec_eth:sin12 + agec_eth:cos12")
 
 eqn.Zb <-as.formula("~ -1 + agec_eth:RSV ")
 eqn.Zc <-as.formula("~ -1 + agec_eth:hMPV ")
 eqn.Zd <-as.formula("~ -1 + agec_eth:influenza.a +agec_eth:influenza.b ")
 eqn.Ze <-as.formula("~ -1 + agec_eth:respiratory.adenovirus ")
 eqn.Zf <-as.formula("~ -1 + agec_eth:parainfluenza ")

 eqn.Z.vir <-as.formula("~ -1 +agec_eth:RSV + agec_eth:hMPV + agec_eth:influenza.a +agec_eth:influenza.b + agec_eth:respiratory.adenovirus+ agec_eth:parainfluenza ")
 
  
  Za <- model.matrix(eqn.Za, data=ds)
  Zb <- model.matrix( eqn.Zb, data=ds)
  Zc <- model.matrix( eqn.Zc, data=ds)
  Zd <- model.matrix( eqn.Zd, data=ds)
  Ze <- model.matrix( eqn.Ze, data=ds)
  Zf <- model.matrix( eqn.Zf, data=ds)
  
  Z.virus <- cbind(Zb, Zc, Zd, Ze,Zf)
  
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
  
  Zb.norsv <- model.matrix(eqn.Zb, data=ds.norsv)
  Zc.nohmpv <- model.matrix(eqn.Zc, data=ds.nohmpv)
  Zd.noflu <- model.matrix(eqn.Zd, data=ds.noflu)
  
  Ze.noadeno <- model.matrix(eqn.Ze, data=ds.noadeno)
  Zf.noparaflu <- model.matrix(eqn.Zf, data=ds.noparaflu)
 
  Z.novirus <- model.matrix(eqn.Z.vir, data=ds.novirus)

  Z.noRSV <- cbind(Zb.norsv, Zc, Zd, Ze,Zf)
  Z.noHMPV <- cbind(Zb, Zc.nohmpv, Zd, Ze,Zf)
  Z.noflu <- cbind(Zb, Zc, Zd.noflu, Ze,Zf)
  Z.noadeno <- cbind(Zb, Zc, Zd, Ze.noadeno,Zf)
  Z.noHMPV <- cbind(Zb, Zc.nohmpv, Zd, Ze,Zf)
  Z.noparaflu <- cbind(Zb, Zc, Zd, Ze,Zf.noparaflu)
  

  denom = ds$pop/1000

  #NOTE priors on precision are for LOG precision. so prec.intercept=0 is exp(0)=1
  prior.fixed <- list(mean.intercept= 0, prec.intercept = log(1),
                      mean = 0, prec =  log(1))

  
   y <- ds[,outcome.var]/denom
   
   mean.y <- mean(y, na.rm=T)
   sd.y <- sd(y, na.rm=T) 
   y.scale <- (y - mean.y)/sd.y

   formula <- y.scale ~ -1  + X + f(idx.Za, model="z", Z=Za) +  
     f(idx.Zb, model="z", Z=Zb) +
     f(idx.Zc, model="z", Z=Zc) +
     f(idx.Zd, model="z", Z=Zd) +
     f(idx.Ze, model="z", Z=Ze) +
     f(idx.Zf, model="z", Z=Zf)
   
   n <- nrow(ds)
   
  mod.inla2 <- inla(formula, data = list(y.scale=y.scale, idx.Za = 1:n,idx.Zb = 1:n,
                                         idx.Zc = 1:n,
                                         idx.Zd = 1:n,
                                         idx.Ze = 1:n,
                                         idx.Zf = 1:n,
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
  Zb.test <- list('all'=Z.virus, 'NoFlu'=Z.noflu, 'NoRSV'=Z.noRSV, 'noHMPV'=Z.noHMPV, 'NoVirus'=Z.novirus)
  res1 <- pbmapply(FUN=gen_pred_interval_inla_ridge_ar1, X1=X.test,Zb=Zb.test,  MoreArgs=list(inla_obj=mod.inla2, covar.df=ds,Za=Za, outcome_name=plot.var,offset1= denom, sd.y=sd.y, mean.y=mean.y), SIMPLIFY=F)
  
  p1 <- Plot_Obs_exp_counterfact(ds=res1,plot.var=plot.var)
  
  attrib_pct = attrib_pct_func(ds=res1)
  preds.inla2= list('res1'=res1, 'waic'=waic, 'dic'=dic, 'obs_exp_plot'=p1$obs_exp, 'predict_plot'=p1$predict.plot, 'attrib_pct'=attrib_pct)
  return(preds.inla2)
}