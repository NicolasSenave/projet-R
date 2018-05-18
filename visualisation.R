visualisation <- function(n,delta,beta,x){
  prob <- f_logit(x,delta,beta)
  P <- c(prob, 1-prob)
  pie(P,main="Camembert de la repartiton theorique de Y",labels=c(1,0), col=c("blue","orange"))
}
