all1.m <- reshape2::melt(all1[,c('ethnicity','date','agec','CAAP','influenza.a','influenza.b','RSV','hMPV',
                                 'respiratory.adenovirus','parainfluenza')] , id.vars=c('ethnicity','date','agec'))

all1.c <- acast(all1.m, date ~ ethnicity~ agec~variable)
str(all1.c)

Y.scale <- (all1.c[,,,'CAAP'] - mean(all1.c[,,,'CAAP']))/ sd(all1.c[,,,'CAAP'])
rsv <- all1.c[,,,'RSV'] / max(all1.c[,,,'RSV'])
fluA <- all1.c[,,,'influenza.a'] / max(all1.c[,,,'influenza.a'])
fluB <- all1.c[,,,'influenza.b'] / max(all1.c[,,,'influenza.b'])
hmpv <- all1.c[,,,'hMPV'] / max(all1.c[,,,'hMPV'])
adeno <- all1.c[,,,'respiratory.adenovirus'] / max(all1.c[,,,'respiratory.adenovirus'])
paraflu <- all1.c[,,,'parainfluenza'] / max(all1.c[,,,'parainfluenza'])

N.times <- dim(all1.c)[1]
t <- 1:N.times
sin12 <- sin(2*pi*t/12)
cos12 <- sin(2*pi*t/12)
t.scale <- t/max(t)

model_string<-"
    model{
         
    for(t in 1:Ntimes){ #time
     for(i in 1:2){ #ethnicity
       for(j in 1:3){ #age
    
    
    y[t,i,j] ~ dnorm(mu[t,i,j], tau)
    
    mu[t,i,j] <-  beta[1] + b[1,i,j] + 
                  beta[2]*sin12[t] + b[2,i,j]*sin12[t] + 
                  beta[3]*cos12[t]  + b[3,i,j]*cos12[t] + 
                  beta[4]*t.scale[t] +  b[4,i,j]*t.scale[t] + 
                  beta[5]*rsv[t,i,j] + b[5,i,j]*rsv[t,i,j] +
                  beta[6]*hmpv[t,i,j] +  b[6,i,j]*hmpv[t,i,j] +
                  beta[7]*flua[t,i,j] +  b[7,i,j]*flua[t,i,j] +
                  beta[8]*flub[t,i,j] +  b[8,i,j]*flub[t,i,j] +
                  beta[9]*adeno[t,i,j] +  b[9,i,j]*adeno[t,i,j] +
                  beta[10]*paraflu[t,i,j] +  b[10,i,j]*paraflu[t,i,j] 
                  
        mu.no.rsv[t,i,j] <- mu[t,i,j] - (beta[5]*rsv[t,i,j] + b[5,i,j]*rsv[t,i,j] )
        mu.no.hmpv[t,i,j] <- mu[t,i,j] - (beta[6]*hmpv[t,i,j] +  b[6,i,j]*hmpv[t,i,j] )
        mu.no.flu[t,i,j] <- mu[t,i,j] - ( beta[7]*flua[t,i,j] +  b[7,i,j]*flua[t,i,j] +beta[8]*flub[t,i,j] +  b[8,i,j]*flub[t,i,j] )
        mu.no.adeno[t,i,j] <- mu[t,i,j] - (beta[9]*adeno[t,i,j] +  b[9,i,j]*adeno[t,i,j])
        mu.no.paraflu[t,i,j] <- mu[t,i,j] - ( beta[10]*paraflu[t,i,j] +  b[10,i,j]*paraflu[t,i,j] )
        mu.no.virus[t,i,j] <- beta[1] + b[1,i,j] + 
                  beta[2]*sin12[t] + b[2,i,j]*sin12[t] + 
                  beta[3]*cos12[t]  + b[3,i,j]*cos12[t] + 
                  beta[4]*t.scale[t] +  b[4,i,j]*t.scale[t]
    
       }
     }
    }
    
    
    for(k in 1:10){
      beta[k]~ dnorm(0, 5)
      sd.b[k] ~ dunif(0,100)
      prec.b[k] <- 1/sd.b[k]^2
      
     for(i in 1:2){ #ethnicity
      for(j in 1:3){ #age
      
        b[k,i,j] ~ dnorm(0, prec.b[k] )
    
      }
     }
    }
  tau <- 1/sd.tau^2
  sd.tau ~ dunif(0,100)
             
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
                       data=list('y' = Y.scale,
                                 't.scale'=t.scale,
                                 'sin12'=sin12,
                                 'cos12'=cos12,
                                 'rsv'=rsv,
                                 'hmpv'=hmpv,
                                 'adeno'=adeno,
                                 'paraflu'=paraflu,
                                 'flua'=fluA,
                                 'flub'=fluB,
                                 'Ntimes'=N.times
                                 ),
                       n.adapt=10000, 
                       n.chains=3)

params<-c('mu',
          'mu.no.rsv',
          'mu.no.hmpv',
          'mu.no.flu',
          'mu.no.adeno',
          'mu.no.paraflu',
          'mu.no.virus',
          'sd.b',
          'beta','b'
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
post.mu <- array(posterior_samples.all[, grep('mu[', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))
post.mu.no.rsv <- array(posterior_samples.all[, grep('mu.no.rsv', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))
post.mu.no.hmpv <- array(posterior_samples.all[, grep('mu.no.hmpv', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))
post.mu.no.flu <- array(posterior_samples.all[, grep('mu.no.flu', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))
post.mu.no.adeno <- array(posterior_samples.all[, grep('mu.no.adeno', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))
post.mu.no.paraflu <- array(posterior_samples.all[, grep('mu.no.paraflu', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))
post.mu.no.virus <- array(posterior_samples.all[, grep('mu.no.virus', lab1, fixed=T)],dim=c(3000, dim(Y.scale)))

all.post <- list('post.mu'=post.mu,'post.mu.no.rsv'=post.mu.no.rsv,'post.mu.no.hmpv'=post.mu.no.hmpv,
                'post.mu.no.flu'=post.mu.no.flu, 'post.mu.no.adeno'=post.mu.no.adeno,'post.mu.no.paraflu'=post.mu.no.paraflu
              ,'post.mu.no.virus'=post.mu.no.virus)

res.list<-list('tabletext'=tabletext, 'summary_data'=summary_data,'overall.VE'=overall.VE,'st.VE'=st.VE)
return(res.list)

}