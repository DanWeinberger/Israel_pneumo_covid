plot_Obs_exp_counterfact_mcmc <- function(ds.in=res1.caap.all, plot.var='CAAP', post.ci=F, grayselect='gray'){
  
  ds <- ds.in$agec.preds$fitted
  ds$date <- min(all1$date) %m+% months(ds$time) 
  ds$agec <- as.factor(ds$agec)
  
  obs.ds <- aggregate(all1[,plot.var, drop=F], by=list('agec'=all1$agec,'date'=all1$date) , FUN=sum)
  
  ds <- merge(ds, obs.ds, by=c('agec', 'date'))
  
  ds_novir <-ds.in$agec.preds$Virus
  
  ds_novir$date <- min(all1$date) %m+% months(ds_novir$time) 
  
  ds_novir$agec <- as.character(ds_novir$agec)
  ds_novir$agec[ds_novir$agec=='1'] <- '<12m'
  ds_novir$agec[ds_novir$agec=='2'] <- '12-23m'
  ds_novir$agec[ds_novir$agec=='3'] <- '24-59m'
  ds_novir$agec <- as.factor(ds_novir$agec)
  
  ds$agec <- as.character(ds$agec)
  ds$agec[ds$agec=='1'] <- '<12m'
  ds$agec[ds$agec=='2'] <- '12-23m'
  ds$agec[ds$agec=='3'] <- '24-59m'
  ds$agec <- as.factor(ds$agec)
  
  
  ds_novir$pred.lcl.post <- ds_novir$prec.lcl
  ds_novir$pred.lcl.post[ds_novir$date<'2020-03-01'] <- NA
  
  ds_novir$pred.ucl.post <- ds_novir$pred.ucl
  ds_novir$pred.ucl.post[ds_novir$date<'2020-03-01'] <- NA
 
  ds$start.date <- as.numeric(as.Date('2020-01-01'))
  ds_novir$start.date <- as.numeric(as.Date('2020-01-01'))
  
  p1 <- ggplot(ds, aes(x=date, y=pred)) +
    geom_line( color='black', lty=1)+
    geom_line(data=ds, aes_string(x='date', y=plot.var), color=grayselect, lty=1)+
    geom_line(data=ds_novir, aes(x=date, y=pred), col='red', lty=2)+
    facet_grid( ~agec) +
    #geom_vline(xintercept=ds$start.date[1], lty=2, col='gray')+
    geom_hline(yintercept=0) +
    theme_classic() +
    geom_vline(xintercept=as.numeric(as.Date('2020-03-01')))
  
  if(post.ci==T){
    p1 <- p1 + geom_ribbon(data=ds_novir, aes(x=date, ymin=pred.lcl.post, ymax=pred.ucl.post),alpha=0.1,fill='red', col='white' )
  }else{
    p1 <- p1 + geom_ribbon(data=ds_novir, aes(x=date, ymin=pred.lcl.post, ymax=pred.ucl),alpha=0.1,fill='red', col='white' )
    
  }
  

  p1
  return(p1)
}