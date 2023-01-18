format_caap <- function(){
  c1a <- read_excel('./DONOTSYNC/Bart Adriaan_van der_CAAP and non-CAAP LRI 2016-2021 corrected.xlsx', sheet='<1y')
c1a$agec <- 1
names(c1a) <- gsub('under1y', '', names(c1a))

c1b <- read_excel('./DONOTSYNC/Bart Adriaan_van der_CAAP and non-CAAP LRI 2016-2021 corrected.xlsx', sheet='1y')
c1b$agec <- 2
names(c1b) <- gsub('1y', '', names(c1a))

c1c <- read_excel('./DONOTSYNC/Bart Adriaan_van der_CAAP and non-CAAP LRI 2016-2021 corrected.xlsx', sheet='2-4y')
c1c$agec <- 3
names(c1c) <- gsub('2-4y', '', names(c1a))

c2 <- bind_rows(c1a,c1b,c1c)

c2.m <- melt(c2, id.vars=c('agec','Date'))

c2.m$ethnicity <- NA
c2.m$ethnicity[grep('Jewish',c2.m$variable)] <- "J" 
c2.m$ethnicity[grep('Bedouin',c2.m$variable)] <- "B" 
c2.m$ethnicity[grep('Total',c2.m$variable)] <- "T" 

c2.m$vartype <- NA
c2.m$vartype[grep('nonCAAPLRI',c2.m$variable)] <- "nonCAAP"
c2.m$vartype[grep('pop',c2.m$variable)] <- "pop"
c2.m$vartype[grep('CAAP..',c2.m$variable, fixed=T)] <- "CAAP"
c2.m <- c2.m[-grep('Inc',c2.m$variable),] #get rid of incidence

c2.m <- c2.m[c2.m$ethnicity!='T',]

c3 <- reshape2::dcast(c2.m, agec + ethnicity + Date  ~ vartype)

names(c3)[1:3] <- c('agec','ethnicity','date')

c3 <- c3[c3$date>'2015-12-31',]
return(c3)
}
