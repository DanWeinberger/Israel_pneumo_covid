attrib_pct_fmt_func <- function(ds, label1){
  attrib.pct.any <- ds$attrib_pct$attrib_pct_any
  
  attrib.pct.rsv <- ds$attrib_pct$attrib_pct_RSV
  
  attrib.pct.hmpv <- ds$attrib_pct$attrib_pct_HMPV
  
  attrib.pct.flu <- ds$attrib_pct$attrib_pct_Flu
  
  attrib.pct.df <- round(bind_rows(attrib.pct.any,attrib.pct.rsv,attrib.pct.hmpv,attrib.pct.flu))
  
  attrib.pct.df$virus <- c('any','rsv','hmpv','flu')
  
  attrib.pct.df$attrib_pct_fmt_all_noncaap <- paste(attrib.pct.df$attrib_pct_med, '( ',attrib.pct.df$attrib_pct_lcl,', ', attrib.pct.df$attrib_pct_ucl,')'   )
  
  out.ds <- attrib.pct.df[,c('virus', 'attrib_pct_fmt_all_noncaap')]
  names(out.ds) <- c('virus',label1)
  
  return(out.ds)
}