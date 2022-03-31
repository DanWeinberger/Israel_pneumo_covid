attrib_pct_func <- function(ds){
  preds.iter <- ds$all$preds_covars.m
  names(preds.iter) <- c('agec','ethnicity','date','variable', 'pred')
  
  preds.iter.NoFlu <- ds$NoFlu$preds_covars.m
  names(preds.iter.NoFlu) <- c('agec','ethnicity','date','variable', 'pred_NoFlu')
  
  preds.iter.NoRSV <- ds$NoRSV$preds_covars.m
  names(preds.iter.NoRSV) <- c('agec','ethnicity','date','variable', 'pred_NoRSV')
  
  preds.iter.NoVirus <- ds$NoVirus$preds_covars.m
  names(preds.iter.NoVirus) <- c('agec','ethnicity','date','variable', 'pred_NoVirus')
  
  preds.iter.NoHMPV <- ds$noHMPV$preds_covars.m
  names(preds.iter.NoHMPV) <- c('agec','ethnicity','date','variable', 'pred_NoHMPV')
  
  preds.iter.NoParaflu <- ds$noParaflu$preds_covars.m
  names(preds.iter.NoParaflu) <- c('agec','ethnicity','date','variable', 'pred_NoParaflu')
  
  preds.iter.NoAdeno <- ds$noAdeno$preds_covars.m
  names(preds.iter.NoAdeno) <- c('agec','ethnicity','date','variable', 'pred_NoAdeno')
  
  # preds.iter.NoFlu <- ds$NoFlu$preds_covars.m
  # names(preds.iter.NoFlu) <- c('agec','ethnicity','date','variable', 'pred_NoFlu')
  # 
  
  
  preds.iter2 <- merge(preds.iter, preds.iter.NoVirus, by=c('agec','ethnicity','date','variable'))
  
  preds.iter2 <- merge(preds.iter2, preds.iter.NoFlu, by=c('agec','ethnicity','date','variable'))
  
  preds.iter2 <- merge(preds.iter2, preds.iter.NoRSV, by=c('agec','ethnicity','date','variable'))
  
  preds.iter2 <- merge(preds.iter2, preds.iter.NoHMPV, by=c('agec','ethnicity','date','variable'))

  #preds.iter2 <- merge(preds.iter2, preds.iter.NoFlu, by=c('agec','ethnicity','date','variable'))
  
  #Attributable to any virus
  
  attrib_pct_any <- preds.iter2 %>%
    group_by( variable) %>%
    summarize(pred=sum(pred), pred_NoVirus=sum(pred_NoVirus) ) %>%
    mutate(attrib_pct_Virus = 100*(pred-pred_NoVirus)/pred ) %>%
    ungroup() %>%
    # group_by(agec) %>%
    summarize( attrib_pct_med = median(attrib_pct_Virus), 
               attrib_pct_lcl=quantile(attrib_pct_Virus, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_Virus, 0.975))
  
  attrib_pct_any_age <- preds.iter2 %>%
    group_by( agec,ethnicity,variable) %>%
    summarize(pred=sum(pred), pred_NoVirus=sum(pred_NoVirus) ) %>%
    mutate(attrib_pct_Virus = 100*(pred-pred_NoVirus)/pred ) %>%
    ungroup() %>%
    group_by(agec,ethnicity) %>%
    summarize( attrib_pct_med = median(attrib_pct_Virus), 
               attrib_pct_lcl=quantile(attrib_pct_Virus, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_Virus, 0.975))
 
  
  ###RSV
  attrib_pct_RSV <- preds.iter2 %>%
    group_by( variable) %>%
    summarize(pred=sum(pred), pred_NoRSV=sum(pred_NoRSV) ) %>%
    mutate(attrib_pct_RSV = 100*(pred-pred_NoRSV)/pred ) %>%
    ungroup() %>%
    # group_by(agec) %>%
    summarize( attrib_pct_med = median(attrib_pct_RSV), 
               attrib_pct_lcl=quantile(attrib_pct_RSV, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_RSV, 0.975))
  
  attrib_pct_RSV_age <- preds.iter2 %>%
    group_by( agec,ethnicity,variable) %>%
    summarize(pred=sum(pred), pred_NoRSV=sum(pred_NoRSV) ) %>%
    mutate(attrib_pct_RSV = 100*(pred-pred_NoRSV)/pred ) %>%
    ungroup() %>%
    group_by(agec,ethnicity) %>%
    summarize( attrib_pct_med = median(attrib_pct_RSV), 
               attrib_pct_lcl=quantile(attrib_pct_RSV, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_RSV, 0.975))

  #hMPV
  attrib_pct_HMPV <- preds.iter2 %>%
    group_by( variable) %>%
    summarize(pred=sum(pred), pred_NoHMPV=sum(pred_NoHMPV) ) %>%
    mutate(attrib_pct_HMPV = 100*(pred-pred_NoHMPV)/pred ) %>%
    ungroup() %>%
    # group_by(agec) %>%
    summarize( attrib_pct_med = median(attrib_pct_HMPV), 
               attrib_pct_lcl=quantile(attrib_pct_HMPV, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_HMPV, 0.975))
  
  attrib_pct_HMPV_age <- preds.iter2 %>%
    group_by( agec,ethnicity,variable) %>%
    summarize(pred=sum(pred), pred_NoHMPV=sum(pred_NoHMPV) ) %>%
    mutate(attrib_pct_HMPV = 100*(pred-pred_NoHMPV)/pred ) %>%
    ungroup() %>%
    group_by(agec,ethnicity) %>%
    summarize( attrib_pct_med = median(attrib_pct_HMPV), 
               attrib_pct_lcl=quantile(attrib_pct_HMPV, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_HMPV, 0.975))
  #Flu
  attrib_pct_Flu <- preds.iter2 %>%
    group_by( variable) %>%
    summarize(pred=sum(pred), pred_NoFlu=sum(pred_NoFlu) ) %>%
    mutate(attrib_pct_Flu = 100*(pred-pred_NoFlu)/pred ) %>%
    ungroup() %>%
    # group_by(agec) %>%
    summarize( attrib_pct_med = median(attrib_pct_Flu), 
               attrib_pct_lcl=quantile(attrib_pct_Flu, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_Flu, 0.975))
  
  attrib_pct_Flu_age <- preds.iter2 %>%
    group_by( agec,ethnicity,variable) %>%
    summarize(pred=sum(pred), pred_NoFlu=sum(pred_NoFlu) ) %>%
    mutate(attrib_pct_Flu = 100*(pred-pred_NoFlu)/pred ) %>%
    ungroup() %>%
    group_by(agec,ethnicity) %>%
    summarize( attrib_pct_med = median(attrib_pct_Flu), 
               attrib_pct_lcl=quantile(attrib_pct_Flu, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_Flu, 0.975))
  
  #Adeno
  attrib_pct_Paraflu <- preds.iter2 %>%
    group_by( variable) %>%
    summarize(pred=sum(pred), pred_NoParaflu=sum(pred_NoParaflu) ) %>%
    mutate(attrib_pct_Paraflu = 100*(pred-pred_NoParaflu)/pred ) %>%
    ungroup() %>%
    # group_by(agec) %>%
    summarize( attrib_pct_med = median(attrib_pct_Paraflu), 
               attrib_pct_lcl=quantile(attrib_pct_Paraflu, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_Paraflu, 0.975))
  
  attrib_pct_Paraflu_age <- preds.iter2 %>%
    group_by( agec,ethnicity,variable) %>%
    summarize(pred=sum(pred), pred_NoParaflu=sum(pred_NoParaflu) ) %>%
    mutate(attrib_pct_Paraflu = 100*(pred-pred_NoParaflu)/pred ) %>%
    ungroup() %>%
    group_by(agec,ethnicity) %>%
    summarize( attrib_pct_med = median(attrib_pct_Paraflu), 
               attrib_pct_lcl=quantile(attrib_pct_Paraflu, 0.025), 
               attrib_pct_ucl=quantile(attrib_pct_Paraflu, 0.975))
  
  #Paraflu
 
  
  out.list=list('attrib_pct_HMPV_age'=attrib_pct_HMPV_age, 'attrib_pct_HMPV'=attrib_pct_HMPV,
                'attrib_pct_RSV'=attrib_pct_RSV,'attrib_pct_RSV_age'=attrib_pct_RSV_age, 
                'attrib_pct_any_age'=attrib_pct_any_age,'attrib_pct_any'=attrib_pct_any,
                'attrib_pct_Flu_age'=attrib_pct_Flu_age,'attrib_pct_Flu'=attrib_pct_Flu,
                 'attrib_pct_Paraflu_age'=attrib_pct_Paraflu_age,'attrib_pct_Paraflu'=attrib_pct_Paraflu,
                'attrib_pct_Adeno_age'=attrib_pct_Adeno_age,'attrib_pct_Adeno'=attrib_pct_Adeno)
  }