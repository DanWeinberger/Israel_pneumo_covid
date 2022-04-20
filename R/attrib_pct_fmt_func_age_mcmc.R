attrib_pct_fmt_func_age_mcmc <- function(ds, label1){
  d1 <- mod.results[[ds]]$attrib.pct.agec
  ap.format <- paste0(d1[,3] , ' (' ,d1[,4], ',' , d1[,5], ')' )
  d2 <- cbind.data.frame(d1[,c(1,2)], ap.format)
  names(d2)<- c('agec','Virus',label1)
  return(d2)
}
