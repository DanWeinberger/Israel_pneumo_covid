#Format posteriors from JAGS into array
format_post_func1 <- function(abbrev.subset='mu[', Y.mean, Y.sd){
  scaled.data <- t(posterior_samples.all[, grep(abbrev.subset, lab1, fixed=T)])
  orig.scale.data <- scaled.data*Y.sd + Y.mean
  
  post.mu <- as.data.frame(orig.scale.data)
  
  
  mu.labs <- dimnames(post.mu)[[1]]
  mu.labs.df <- as.data.frame(matrix(unlist(strsplit(sub("\\].*", "", sub(".*\\[", "", mu.labs)) ,',')), ncol=3, byrow=T))
  names(mu.labs.df) <- c('t','ethnicity','agec')
  
  post.mu <- cbind.data.frame(mu.labs.df,post.mu)
  post.mu$t <- as.numeric(post.mu$t)
  
  post.mu.m <- reshape2::melt(post.mu, id.vars=c('t','ethnicity','agec'))
  post.mu.c <- acast(post.mu.m, t~ethnicity~agec ~variable)
  return(post.mu.c)
}
