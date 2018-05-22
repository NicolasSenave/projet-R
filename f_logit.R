###################################################################################
##'
##'
##' Retourne la valeur P(Y = 1 | X = x)
##' ou Y|X = x suit un modele logit de parametres (delta,beta).
##'
##' @param x numeric. x est un reel.
##' @param delta numeric. Parametre delta du modele logistique.
##' @param beta numeric. Parametre beta du modele logistique.
##'
##' @examples
##' f_logit(0.7, 1.0, 2.0)
##'
##' @return numeric.
##'
##'


f_logit <- function(x,delta,beta){
  z = delta + beta*x
  return(1 - 1/(1+exp(z)))
}