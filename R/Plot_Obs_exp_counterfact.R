Plot_Obs_exp_counterfact <- function(ds,plot.var){
  preds_time_all <- ds$all$preds_time
  
  preds_time_NoVirus <- ds$NoVirus$preds_time
  
  preds_time_all$pred_lcl_post <- preds_time_all$pred_lcl
  preds_time_all$pred_lcl_post[preds_time_all$date<'2020-03-01'] <- NA
  
  preds_time_all$pred_ucl_post <- preds_time_all$pred_ucl
  preds_time_all$pred_ucl_post[preds_time_all$date<'2020-03-01'] <- NA
  
  preds_time_all$pred_mean_post <- preds_time_all$pred_mean
  preds_time_all$pred_mean_post[preds_time_all$date<'2020-03-01'] <- NA
  
  preds_time_all$pred_mean_pre <- preds_time_all$pred_mean
  preds_time_all$pred_mean_pre[preds_time_all$date>='2020-03-01'] <- NA
  
  p1 <- ggplot(preds_time_all, aes_string(x='date', y=plot.var)) +
    geom_line(color='gray')+
    geom_line(data=preds_time_all,aes(x=date, y=pred_mean), color='black', lty=2)+
    
    geom_line(data=preds_time_NoVirus, aes(x=date, y=pred_mean), col='red')+
    
    facet_grid(agec~ethnicity) +
    #  coord_cartesian(ylim = c(0, 75)) + 
    geom_hline(yintercept=0) +
    theme_classic()
  
  p2 <- ggplot(preds_time_all, aes_string(x='date', y=plot.var)) +
    geom_line()+
    geom_line(data=preds_time_NoVirus) +
    geom_ribbon( aes(x=date, ymin=pred_lcl_post, ymax=pred_ucl_post),alpha=0.1, col='white' )+
    facet_grid(agec~ethnicity) +
    geom_line(aes_string(x='date', y=plot.var))+
    geom_line(aes(x=date, y=pred_mean_pre), color='black', lty=2)+
    geom_line(aes(x=date, y=pred_mean_post), color='black', lty=2)+
    #  coord_cartesian(ylim = c(0, 75)) + 
    geom_hline(yintercept=0) +
    theme_classic()
return(plots=list('obs_exp'=p1, 'predict.plot'=p2))
}
