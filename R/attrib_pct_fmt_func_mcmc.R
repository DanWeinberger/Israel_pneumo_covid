attrib_pct_fmt_func_mcmc <- function(ds, label1){
  d1 <- mod.results[[ds]]$attrib.pct.overall
  ap.format <- paste0(d1[,1] , ' (' ,d1[,2], ',' , d1[,3], ')' )
  d2 <- cbind.data.frame(rownames(d1), ap.format)
  names(d2)<- c('Virus',label1)
  return(d2)
}
