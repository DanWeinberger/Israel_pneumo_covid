format_ipd_func <- function(){
  ipd1 <- read_excel('./DONOTSYNC/Bart Adriaan_van der_IPD 2016-2021.xlsx')
  
  ipd1 <- ipd1[, c('Date',"other.IPD.under1y.Jewish","other.IPD.under1y.nonJewish","bacteremic.pneumonia.under1y.Jewish","bacteremic.pneumonia.under1y.nonJewish","Jewish.pop.under1y" ,"nonJewish.pop.under1y")]
  
  ipd1.m <- melt(ipd1, id.vars = 'Date')
  
  ipd1.m$ethnicity= 'J'
  ipd1.m$ethnicity[grep('nonJewish',ipd1.m$variable)] <- 'N'
  
  ipd1.m$outcome= 'IPD_pneumonia'
  ipd1.m$outcome[grep('other.IPD',ipd1.m$variable)] <- 'IPD_other'
  ipd1.m$outcome[grep('pop',ipd1.m$variable)] <- 'pop'
  
  ipd1.c <- reshape2::dcast(ipd1.m,Date+ethnicity~outcome)
  ipd1.c$ethnicity <- as.factor(ipd1.c$ethnicity)
  
  p.ipd_pneu <- ggplot(ipd1.c, aes(x=Date, y=IPD_pneumonia, group=ethnicity, colour=ethnicity)) +
    geom_line() +
    theme_classic()
  
  p.ipd_other <- ggplot(ipd1.c, aes(x=Date, y=IPD_other, group=ethnicity, colour=ethnicity)) +
    geom_line() +
    theme_classic()
  out.list=list('ds'=ipd1.c,'p.ipd_pneu'=p.ipd_pneu,'p.ipd_other'=p.ipd_other)
  return(out.list)
}