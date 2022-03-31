gen_pred_interval_inla_ridge_ar1 <- function(X1,Zb,inla_obj, Za,  covar.df, outcome_name, source,offset1=NULL){
  
  nrep1=1000
  nrep2=1
  
  mod.mat.comb <- cbind(X1, Za, Zb)
  
  r.samples = inla.posterior.sample(nrep1, inla_obj)
  
  all.names <- dimnames(r.samples[[1]]$latent)[[1]] 
  
  X.names <- paste0(dimnames(X1)[[2]], ':1')
  Za.names <- all.names[grep('idx.Za', all.names) ][-c(1:nrow(Za))] 
  Zb.names <- all.names[grep('idx.Zb', all.names) ][-c(1:nrow(Zb))] 
  t.names <- all.names[which(substring(all.names,1,2)=='t:')] #ar1 component from training data
  
  
  #AR1.obs is fit to the data pre-pandemic, then extrapolate after
  # ar1.obs <- sapply(r.samples, function(x){
  #   x$latent[t.names,, drop=F]
  # })
  # #matplot(ar1.obs, type='l')
  # 
  # ar1.obs <- as.data.frame(ar1.obs)
  # names(ar1.obs) <- paste0('sampleAR', 1:ncol(ar1.obs))
  # 
  # ar1.obs$date <- sort(unique(covar.df$date))
  
  
  pred.names <- c(X.names,Za.names,Zb.names)
  # pred.names.check <- gsub(':1','', pred.names)
  #  sum(pred.names.check==colnames(mod.mat)) #order is correct
  
  sample.ds1 <- sapply(r.samples, function(x){
    betas <- x$latent[pred.names,, drop=F]

    pred1 <- as.vector(mod.mat.comb %*% betas) 

    return(pred1)
  }, simplify='array')
  
   
  sample.ds1 <- as.data.frame(sample.ds1)
  sample.ds1$date <- covar.df$date
  
  #covar.ar <- merge(sample.ds1, ar1.obs, by='date', sort=F) #ensure everything correctly matched by date
  
  sample.ds1$date <- NULL 
  
  #covar.ar <- covar.ar[,grep('sampleAR', names(covar.ar)) ]
  
  sample.ds2a <- sample.ds1  #+ ( covar.ar)

  sample.ds2 <- apply(sample.ds2a,2, function(x) x*offset1 )

  mod.family <- inla_obj$.args$family
  
  sample.ds1.m <- melt(as.matrix(sample.ds2)) #vectorize...
  
  sample.ds1.m.rep <- sapply(sample.ds1.m, rep.int,times=nrep2)
  
  
  mod.sd <- 1/sqrt(inla_obj$summary.hyperpar['Precision for the Gaussian observations','mean'])
    #pred <- rnorm(n=length(sample.ds1.m.rep[,3]), mean=sample.ds1.m.rep[,3] , sd=mod.sd)
  
  ##DO NOT NEED TO RESAMPLE FROM NORMAL BC ONLY CARE ABOUT PARAMETER UNCERTAINTY HERE
  pred <- sample.ds1.m.rep[,3]
  
  
  pred.df <- cbind.data.frame('id1'=sample.ds1.m.rep[,1],'rep'=rep(1:(nrep1*nrep2) , each=nrow(X1)), 'pred'=pred)
  
  posterior.preds.c <- reshape2::dcast(pred.df, id1~rep, value.var='pred')
  posterior.preds.c$id1 <- NULL
  
  colnames(posterior.preds.c) <- paste0('ColA', 1: ncol(posterior.preds.c))
  posterior.preds.df <- cbind.data.frame(covar.df, posterior.preds.c)
  
  #Pred by date
  preds.summary <- (t(apply(posterior.preds.c,1, quantile, probs=c(0.025,0.5,0.975))))
  preds.summary <- as.data.frame(preds.summary)
  names(preds.summary) <- c('pred_lcl','pred_mean','pred_ucl')
  preds.summary <- cbind.data.frame(preds.summary,covar.df)

  p1 <- ggplot(preds.summary, aes(x=date, y=pred_mean))+
    geom_line(col='gray')+
    geom_line(data=preds.summary, aes(x=date, y=CAAP)) +
    theme_classic() +
    facet_grid(agec~ethnicity)
    p1

    
    #sex, race_recode, agec
  preds_covars <- cbind.data.frame(posterior.preds.c, covar.df[,c('agec','ethnicity', 'date',outcome_name)])
  preds_covars.m <- melt(preds_covars, id.vars=c('agec','ethnicity', 'date'))
  
  out.list= list('preds_time'=preds.summary,'preds_covars.m'=preds_covars.m)
  return(out.list)
}



