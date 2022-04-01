attrib_pct_fmt_func_age <- function(ds, label1){
  attrib.pct.any <- ds$attrib_pct$attrib_pct_any_age
  
  attrib.pct.rsv <- ds$attrib_pct$attrib_pct_RSV_age
  
  attrib.pct.hmpv <- ds$attrib_pct$attrib_pct_HMPV_age
  
  attrib.pct.flu <- ds$attrib_pct$attrib_pct_Flu_age
  
  attrib.pct.adeno <- ds$attrib_pct$attrib_pct_Adeno_age
  
  attrib.pct.Paraflu <- ds$attrib_pct$attrib_pct_Paraflu_age
  
  
  attrib.pct.df <- bind_rows(attrib.pct.any,attrib.pct.rsv,attrib.pct.hmpv,attrib.pct.flu,attrib.pct.adeno,attrib.pct.Paraflu)
  
  attrib.pct.df$attrib_pct_med <- round(attrib.pct.df$attrib_pct_med)
  attrib.pct.df$attrib_pct_ucl <- round(attrib.pct.df$attrib_pct_ucl)
  attrib.pct.df$attrib_pct_lcl <- round(attrib.pct.df$attrib_pct_lcl)
  
  combos <- unique(attrib.pct.df[,c('agec','ethnicity')])
  attrib.pct.df$virus <- rep(c('any','rsv','hmpv','flu', 'adeno', 'paraflu'), each=nrow(combos))
  
  attrib.pct.df$attrib_pct_fmt_all_noncaap <- paste(attrib.pct.df$attrib_pct_med, '(',attrib.pct.df$attrib_pct_lcl,', ', attrib.pct.df$attrib_pct_ucl,')'   )
  
  out.ds <- attrib.pct.df[,c('agec','ethnicity','virus', 'attrib_pct_fmt_all_noncaap')]
  names(out.ds) <- c('agec','ethnicity','virus',label1)
  
  return(out.ds)
}
