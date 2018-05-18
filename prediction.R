prediction <- function(x,delta,beta){
  P <- f_logit(x,delta,beta)
  Y <- round(P,0)
  return(rbind(P,Y))
}
