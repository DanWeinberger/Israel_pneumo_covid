fitted_ci_mcmc <- function(fitted.ds, agg.level='overall'){
  if(agg.level=='overall'){
    agg.level <- c(1,4)
  }else if(agg.level=='agec'){
    agg.level <- c(1,3,4)
  }
  else if(agg.level=='ethnicity'){
    agg.level <- c(1,2,4)
  }
  
  pred <- apply(fitted.ds, agg.level, sum)


  if(length(agg.level)==2){
    med.pred <- apply(pred,1,median,credMass = 0.95 )
    hdi.pred <- t(apply(pred,1,hdi,credMass = 0.95 ))
    pred.comb <- cbind(med.pred,hdi.pred)
  }else{
    med.pred <- apply(pred,c(1,2),median,credMass = 0.95 )
    hdi.pred <- apply(pred,c(1,2),hdi,credMass = 0.95 )
    hdi.pred2 <- aperm(hdi.pred, c(2,3,1))
    pred1 <- abind(med.pred,hdi.pred2, along=3)
    pred1.m <- reshape2::melt(pred1)
    pred.comb <- reshape2::dcast(pred1.m, Var1+Var2~Var3)
    names(pred.comb) <- c('time','agec','pred','prec.lcl','pred.ucl')
    }
  
  return(pred.comb)
}