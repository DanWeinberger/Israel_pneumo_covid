format_virus <- function(){
  vir1a <- read_excel('./DONOTSYNC/Bart Adriaan_van der_All Viruses 2016-2021.xlsx',guess_max =50000)
  vir1b <- read_excel('./DONOTSYNC/All Viruses 2022.xlsx',guess_max =50000)
  
  vir1 <- bind_rows(vir1a, vir1b)
  
  vir1$date <- as.Date(paste(vir1$year,vir1$month,'01', sep='-'))
  
  vir1$one <- 1
  
  vir1$agec<- NA
  
  vir1$agec[vir1$age.months<12] <- 1
  
  vir1$agec[vir1$age.months<24 & vir1$age.months>=12] <- 2
  
  vir1$agec[vir1$age.months>=24 ] <- 3
  
  vir1.m <- melt(vir1[c('date','agec','ethnicity','RSV','influenza.a', 'influenza.b','hMPV','parainfluenza','respiratory.adenovirus','rhinovirus')], id.vars=c('date', 'agec','ethnicity') )
  
  vir1.m$pos <- NA
  vir1.m$pos[vir1.m$value=='NEG'] <-0
  vir1.m$pos[vir1.m$value=='POS'] <-1
  
  vir1.m <- vir1.m[!is.na(vir1.m$pos),] #eliminate missings
  
  vir2 <- vir1.m %>%
    group_by(date, ethnicity, variable) %>%
    summarize(N_pos=sum(pos), N_test=n() ) %>%
    mutate(prop=N_pos/N_test) %>%
    ungroup()
  
  p2 <- ggplot(vir2, aes(x=date, y=prop, group=ethnicity, col=ethnicity)) +
    geom_line()+
    theme_classic() +
    facet_wrap(~variable, scales='free')

    vir2.c <- reshape2::dcast(vir2,date+ethnicity~variable, value.var='prop')
  
  out.list=list('virus.plot'=p2,'vir2.c'=vir2.c)
  
}