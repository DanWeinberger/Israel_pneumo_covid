format_carr <- function(){
  carr1 <- read_excel('./DONOTSYNC/Bart Adriaan_van der_Carriage plus Density 2016-2021.xlsx',guess_max=20000)
  
  carr1$date <- as.Date(paste(carr1$year,carr1$month,'01', sep='-'))
  
  carr1$one <- 1
  
  carr1$density <- carr1$quantitative.density.1
  
  carr1$density[carr1$result.1=='NEG'] <- 0
  
  carr1$agec<- NA
  
  carr1$agec[carr1$age.months<12] <- 1
  
  carr1$agec[carr1$age.months<24 & carr1$age.months>=12] <- 2
  
  carr1$agec[carr1$age.months>=24 ] <- 3
  
  carr1$high_inv <- NA
  
  carr1$high_inv[carr1$serotype.1 %in% c('19B','10F','31','17F','20','7C','34','13','24A','23A','28A','19C','11B','7A','23B','35A','35C','37','12A','11A','42','33B','33D','9L','25A','10C','32','18B','15F','11C','18F','33C','22A','25F','39','28F','35F','21','16F','6C','35B','29','6D','15A','9A','19F','18A','46','15BC','6A','23F','9N','6B','4'
                                         ,'10B','9V','7B','14','10A','15B','15C')
  ] <- 0
  
  carr1$high_inv[carr1$serotype.1 %in% c('1','2','5','7F','12F','3','8','33F','27','33F','24F','19A','38','24B','18C','33A','22F')] <- 1
  
  
  carr1.m <- melt(carr1[,c('date','density','one')], id.vars = c('date', 'density') )
  
  carr1.c <- dcast(carr1.m, date ~ density, fun.aggregate = sum)
  
  carr1.c$any <- apply(carr1.c[,2:7],1, sum)
  
  carr1.c$prev.low.density <- (carr1.c$`1`+carr1.c$`2`)/carr1.c$any
  
  carr1.c$prev.hi.density <- (carr1.c$`3` + carr1.c$`4`)/carr1.c$any
  
  all.dates <- cbind.data.frame('date'=seq.Date(from=min(carr1.c$date),
                                                to=max(carr1.c$date), by='month'))
  
  carr1.c <- merge(carr1.c, all.dates, all=T)
  
  carr1.c.p <- reshape2::melt(carr1.c, id.vars='date')
  p1 <- ggplot(carr1.c.p[carr1.c.p$variable %in% c('prev.low.density','prev.hi.density'),], aes(x=date, y=value, group=variable, color=variable)) +
    geom_line() +
    theme_classic()+
    ylab('Prevalence')
  
  #Stratified by age and ethnicity
  carr2.m <- melt(carr1[,c('date','agec','ethnicity','density','result.1')], id.vars = c('date' ,'agec','ethnicity', 'result.1','density') )
  
  carr2.c <- dcast(carr2.m, date+agec+ethnicity ~ result.1, fun.aggregate = length)
  
  carr2.c$prev <- carr2.c$POS/(carr2.c$POS+carr2.c$NEG)

  carr2.c <- carr2.c %>%
    tidyr::complete('date'=seq.Date(min(carr2.c$date, na.rm=T), max(carr2.c$date, na.rm=T), 'month'), 
                    agec=unique(carr2.c$agec),
                    ethnicity=unique(carr2.c$ethnicity),
                    fill=list(POS=NA, NEG=NA, prev=NA))
  carr2.c$prev[is.na(carr2.c$POS)] <- NA
  
  out.list <- list('plot.density'=p1, 'carr2.c'=carr2.c)
    return(out.list)
}