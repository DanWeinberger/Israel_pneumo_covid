attrib_pct_age_mcmc <- function(novir.ds,fitted.ds, agg.level='overall'){
  if(agg.level=='overall'){
    agg.level <- c(4)
  }else if(agg.level=='agec'){
    agg.level <- c(3,4)
  }
  else if(agg.level=='ethnicity'){
    agg.level <- c(2,4)
  }
  
  post.mu.sum <- apply(fitted.ds, agg.level, sum)
  post.novir.sum <- apply(novir.ds, agg.level, sum)
  
  post.attrib.pct <- (post.mu.sum -post.novir.sum) / post.mu.sum
  
  if(length(agg.level)==1){
    attrib_pct <-  round(100* c(median(post.attrib.pct),hdi(post.attrib.pct, credMass = 0.95)))
    }else{
      med.ap <- apply(post.attrib.pct,1,median )
      hdi.ap <- t(apply(post.attrib.pct,1,hdi,credMass = 0.95 ))
      attrib_pct <-  round(100*cbind(med.ap, hdi.ap))
    }
  
  return(attrib_pct)
}