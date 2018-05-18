f_logit <- function(x,delta,beta){
  z=(delta+beta*x)
  return(1-1/(1+exp(z)))
}
