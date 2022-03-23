
lm.mod.fit.func <- function(ds, outcome.var, covars, plotvar='CAAP'){
  
  eqn<- as.formula(paste0(outcome.var,' ~ t +sin12 +cos12 +', paste(covars, collapse='+') )            )

  mod1 <- lm( eqn, data=ds)
  
  summary(mod1)
  
  df1 <- ds
  
  df1$pred <- predict(mod1, newdata=ds)
  
  mod.ds.novir <- ds
  mod.ds.novir$RSV <- 0
  mod.ds.novir$hMPV <- 0
  mod.ds.novir$influenza.a <- 0
  mod.ds.novir$influenza.b <- 0
  
  df1$pred.novirus <- predict(mod1, newdata=mod.ds.novir)
  
  
  
  p4 <-   ggplot(df1, aes_string(x='date', y=plotvar))+
    geom_line()+
    geom_line(data=df1, aes(x=date, y=pred), col='red', lty=2)+
    geom_line(data=df1, aes(x=date, y=pred.novirus), col='blue', lty=2)+
    theme_classic()+
    facet_wrap(ethnicity~agec) 
  
  attrib.pct <- df1 %>%
    summarize(pred=sum(pred), pred.novirus=sum(pred.novirus)) %>%
    mutate(attrib.pct= 100*(pred -pred.novirus)/pred )
  
  out.list=list('obs.exp.plot'=p4, 'attrib.pct'=attrib.pct, 'df1'=df1,'mod'=mod1, 'AIC'=AIC(mod1))
  
  return(out.list)
}