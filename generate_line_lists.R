library(reshape2)
timepoints <- 30
N.index <- 5
N.contact <- 200

T <- array(0, dim=c(timepoints, N.index, N.contact ))


#Contact 1:uninfected
N.index <- 3
T[1:4,1,1] <- 1:4
T[1:5,2,1] <- 1:5
T[1:3,3,1] <- 1:3

 df1 <- as.data.frame(matrix(NA, nrow=3, ncol=5))
 names(df1) <- c('contactID', 'indexID', 'date.index.sympt','date.index.isolated', 'infected')
 df1$contactID <- 1
 df1$indexID <- 1:3
 df1$date.index.sympt <- as.Date(c('2020-03-19','2020-03-23', '2020-03-25'))
 df1$date.index.isolated <- as.Date(c('2020-03-22','2020-03-27', '2020-03-27'))
 
 df1$fu.time <- as.numeric(df1$date.index.isolated - df1$date.index.sympt) +1
 df1$infected <-0
 df1$tf <- max(df1$date.index.isolated) - min(df1$date.index.sympt) +1

 
 
#Contact2 infected 
 df2 <- as.data.frame(matrix(NA, nrow=2, ncol=5))
 names(df2) <- c('contactID', 'indexID', 'date.index.sympt','date.index.isolated', 'infected')
 df2$contactID <- 2
 df2$indexID <- 1:2
 df2$date.index.sympt <- as.Date(c('2020-03-19','2020-03-23'))
 df2$date.index.isolated <- as.Date(c('2020-03-22','2020-03-25'))
 
 df2$fu.time <- as.numeric(df2$date.index.isolated - df2$date.index.sympt) + 1
 df2$infected <-0
 df2$tf <- max(df2$date.index.isolated) - min(df2$date.index.sympt) +1
 
  #Combine data frames from all people
 df.all <- rbind.data.frame(df1, df2)
 df.all.m <- melt(df.all, id.vars = c("contactID","indexID" , 'tf' ))
 
  