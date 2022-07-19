jags_poisson_func <- function(ds=all1,outcome.var='CAAP', plot.var='CAAP'){
  
all1.m <- reshape2::melt(ds[,c('ethnicity','date','agec',outcome.var,'influenza.a','influenza.b','RSV','hMPV',
                                 'respiratory.adenovirus','parainfluenza')] , id.vars=c('ethnicity','date','agec'))

all1.c <- acast(all1.m, date ~ ethnicity~ agec~variable)
str(all1.c)

Y.scale <- (all1.c[,,,outcome.var] - mean(all1.c[,,,outcome.var],na.rm=T))/ sd(all1.c[,,,outcome.var], na.rm=T)

Y=all1.c[,,,outcome.var]

Y.mean <- mean(all1.c[,,,outcome.var] , na.rm=T) 

Y.sd <- sd(all1.c[,,,outcome.var], na.rm=T)

#SCALLE ALL VIRUSES TO BE [0,1]
rsv <- all1.c[,,,'RSV'] / max(all1.c[,,,'RSV'])
fluA <- all1.c[,,,'influenza.a'] / max(all1.c[,,,'influenza.a'])
fluB <- all1.c[,,,'influenza.b'] / max(all1.c[,,,'influenza.b'])
hmpv <- all1.c[,,,'hMPV'] / max(all1.c[,,,'hMPV'])
adeno <- all1.c[,,,'respiratory.adenovirus'] / max(all1.c[,,,'respiratory.adenovirus'])
paraflu <- all1.c[,,,'parainfluenza'] / max(all1.c[,,,'parainfluenza'])

out1 <- all1.c[,1,1,outcome.var]
N.times.fit <- length(out1[!is.na(out1)])
N.times.tot <- length(out1)

t <- 1:N.times.tot
sin12 <- sin(2*pi*t/12)
cos12 <- cos(2*pi*t/12)
t.scale <- t/max(t)

model_string<-"
    model{
         
    for(t in 1:N.times.fit){ #time
     for(i in 1:2){ #ethnicity
       for(j in 1:3){ #age
        #y[t,i,j] ~ dpois(mu[t,i,j])
        
         y[t,i,j]  ~ dnegbin(prob[t,i,j],r)
         prob[t,i,j]<- r/(r+mu[t,i,j])  ## likelihood 
        
        #mu[t,i,j] <- mu.scale[t,i,j]*Y.sd + Y.mean
       }
     }
    }
    
    for(t in 1:Ntimes){ #time
     for(i in 1:2){ #ethnicity
       for(j in 1:3){ #age
    
    mu[t,i,j] <-  ( b[1,i,j] + 
                    b[2,i,j]*sin12[t] + 
                    b[3,i,j]*cos12[t] + 
                    b[4,i,j]*t.scale[t] + 
                    b[5,i,j]*rsv[t,i,j] +
                    b[6,i,j]*hmpv[t,i,j] +
                    b[7,i,j]*flua[t,i,j] +
                    b[8,i,j]*flub[t,i,j] +
                    b[9,i,j]*adeno[t,i,j] +
                    b[10,i,j]*paraflu[t,i,j] )
                  
        mu.no.rsv[t,i,j] <- ( mu[t,i,j] - (b[5,i,j]*rsv[t,i,j] )) 
        
        mu.no.hmpv[t,i,j] <-( mu[t,i,j] - ( b[6,i,j]*hmpv[t,i,j] ))
        mu.no.flu[t,i,j] <- (mu[t,i,j] - (  b[7,i,j]*flua[t,i,j]  +  b[8,i,j]*flub[t,i,j] ))
        
        mu.no.adeno[t,i,j] <- (mu[t,i,j] - (  b[9,i,j]*adeno[t,i,j]))
        
        mu.no.paraflu[t,i,j] <- (mu[t,i,j] - (   b[10,i,j]*paraflu[t,i,j] ))
        
        mu.no.virus[t,i,j] <-   (  b[1,i,j] + 
                   b[2,i,j]*sin12[t] + 
                   b[3,i,j]*cos12[t] + 
                   b[4,i,j]*t.scale[t] )
    
       }
     }
    }
    
    
 
      for(k in 2:4){ #trend and harmonics
          beta[k]~ dnorm(0, 1e-4)
          sd.b[k] ~ dunif(0,100)
          prec.b[k] <- 1/sd.b[k]^2
       for(i in 1:2){ #ethnicity
        for(j in 1:3){ #age
         delta[k,i,j] ~ dnorm(beta[k], prec.b[k] )
        b[k,i,j] <-  delta[k,i,j]  #Do NOT restrict trend or harmonics to be  >0 
    
      }
     }
      }
    
      for(k in c(1,5:10)){
           beta[k]~ dnorm(0, 1) #gets expontiated so N(0,1e-4) prior doesn't make a ton of sense
          sd.b[k] ~ dunif(0,100)
          prec.b[k] <- 1/sd.b[k]^2
       for(i in 1:2){ #ethnicity
        for(j in 1:3){ #age
        delta[k,i,j] ~ dnorm(beta[k], prec.b[k] )
        b[k,i,j] <- exp(delta[k,i,j] ) 
    
        }
       }
      }
    
   r ~ dunif(0,250)

             
    }
"



##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')


##############################################
#Model Organization
##############################################
model_spec<-textConnection(model_string)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1,inits2, inits3),
                       data=list('y' = Y,
                                 't.scale'=t.scale,
                                 'sin12'=sin12,
                                 'cos12'=cos12,
                                 'rsv'=rsv,
                                 'hmpv'=hmpv,
                                 'adeno'=adeno,
                                 'paraflu'=paraflu,
                                 'flua'=fluA,
                                 'flub'=fluB,
                                 'Ntimes'=N.times.tot,
                                 'N.times.fit'=N.times.fit
                                 ),
                       n.adapt=10000, 
                       n.chains=3)

params<-c('mu','r',
          'mu.no.rsv',
          'mu.no.hmpv',
          'mu.no.flu',
          'mu.no.adeno',
          'mu.no.paraflu',
          'mu.no.virus',
          'sd.b',
          'beta','b', 'delta'
          )

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000)

posterior_samples.all<-do.call(rbind,posterior_samples)

#post1.summary<-summary(posterior_samples)
#post_means<-colMeans(posterior_samples.all)
post_means<-apply(posterior_samples.all, 2, median)

sample.labs<-names(post_means)

ci<-t(hdi(posterior_samples.all, credMass = 0.95))

ci<-matrix(sprintf("%.1f",round(ci,1)), ncol=2)

row.names(ci)<-sample.labs

post_means<-sprintf("%.1f",round(post_means,1))

names(post_means)<-sample.labs


lab1 <- dimnames(posterior_samples.all)[[2]]

abbrevs.extract <- c('mu[','mu.no.virus','mu.no.rsv','mu.no.hmpv','mu.no.flu','mu.no.adeno','mu.no.paraflu')

all.post.mu <- pblapply(abbrevs.extract, format_post_func1 , Y.mean=Y.mean, Y.sd=Y.sd,ds=posterior_samples.all,labs=lab1)

names(all.post.mu) <- c('fitted','Virus','RSV','hMPV','Influenza','Adenovirus','Parainfluenza')

attrib.pct.overall <- t(sapply(all.post.mu[-1],attrib_pct_age_mcmc, fitted.ds=all.post.mu[[1]],  agg.level='overall' ))

attrib.pct.agec.c <- sapply(all.post.mu[-1],attrib_pct_age_mcmc, fitted.ds=all.post.mu[[1]],  agg.level='agec', simplify='array' )
attrib.pct.agec.m <- reshape2::melt(attrib.pct.agec.c)
attrib.pct.agec <- reshape2::dcast(attrib.pct.agec.m, Var1+Var3 ~ Var2)
names(attrib.pct.agec) <- c('agec','Virus','Attrib_pct','lcl','ucl')


agec.preds <- pblapply(all.post.mu,fitted_ci_mcmc,   agg.level='agec' )
overall.preds <- pblapply(all.post.mu,fitted_ci_mcmc,   agg.level='overall' )

# Y.age <- apply(Y,c(1,3), sum)
# plot(Y.age[,1], type='l', col='gray')
# points(agec.preds[[1]][agec.preds[[1]]$agec==1,'pred'], type='l')
# points(agec.preds[['Virus']][agec.preds[['Virus']]$agec==1,'pred'], type='l', col='red')


res.list<-list('agec.preds'=agec.preds, 'overall.preds'=overall.preds,'attrib.pct.agec'=attrib.pct.agec,'attrib.pct.overall'=attrib.pct.overall)

return(res.list)
}