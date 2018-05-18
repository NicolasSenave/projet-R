densite_logit <- function(x, delta,beta){
  beta * exp(delta+x*beta) / (1 + exp(delta+x*beta))^2
}
