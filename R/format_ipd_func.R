format_ipd_func <- function(){
  ipd1a <- read_excel('./DONOTSYNC/Bart Adriaan_van der_IPD 2016-2021.xlsx', sheet='<1y')
  ipd1b <- read_excel('./DONOTSYNC/Bart Adriaan_van der_IPD 2016-2021.xlsx', sheet='1y')
  ipd1c <- read_excel('./DONOTSYNC/Bart Adriaan_van der_IPD 2016-2021.xlsx', sheet='2-4y')

  
  c2 <- bind_rows(ipd1a,ipd1b,ipd1c)
  
  c2.m <- melt(c2, id.vars=c('Date'))
  
  
  c2.m$ethnicity <- NA
  c2.m$ethnicity[grep('.Jewish',c2.m$variable, fixed=T)] <- "J" 
  c2.m$ethnicity[grep('nonJewish',c2.m$variable)] <- "B" #Not actually bedouins, but we will use it
  c2.m$ethnicity[grep('total',c2.m$variable)] <- "T" 
  
  c2.m$agec <- NA
  c2.m$agec[grep('under1y',c2.m$variable)] <- 1
  c2.m$agec[grep('.1y',c2.m$variable, fixed=T)] <- 2
  c2.m$agec[grep('2-4y',c2.m$variable)] <- 3
  
  
  c2.m$vartype <- NA
  c2.m$vartype[grep('total.IPD',c2.m$variable)] <- "total.IPD"
  c2.m$vartype[grep('pop',c2.m$variable)] <- "pop"
  c2.m$vartype[grep('other.IPD',c2.m$variable, fixed=T)] <- "other.IPD"
  c2.m$vartype[grep('bacteremic.pneumonia',c2.m$variable, fixed=T)] <- "bacteremic.pneumonia"
  
  c2.m <- c2.m[-grep('Inc',c2.m$variable),] #get rid of incidence
  
  c2.m <- c2.m[c2.m$ethnicity!='T',]
  
  c2.m <- c2.m[!is.na(c2.m$Date),]
  
  c2.m <- c2.m[!is.na(c2.m$value),]
  
  c3 <- reshape2::dcast(c2.m, agec + ethnicity + Date  ~ vartype)
  
  names(c3)[1:3] <- c('agec','ethnicity','date')
  
  
  p.ipd_pneu <- ggplot(c3, aes(x=date, y=bacteremic.pneumonia, group=ethnicity, colour=ethnicity)) +
    geom_line() +
    theme_classic()+
    facet_wrap(~agec)
  
  p.ipd_other <- ggplot(c3, aes(x=date, y=other.IPD, group=ethnicity, colour=ethnicity)) +
    geom_line() +
    theme_classic()+
    facet_wrap(~agec)
  
  
  out.list=list('ds'=c3,'p.ipd_pneu'=p.ipd_pneu,'p.ipd_other'=p.ipd_other)
  return(out.list)
}