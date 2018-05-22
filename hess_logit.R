###################################################################################
##'
##'
##' Renvoie la hessienne de la log-varisemblance
##' du modele logit de parametres (delta,beta)
##' sur les donnees explicatives x et les donnees d'interet binaires y.
##'
##' @param x numeric. L'echantillon des donnees explicatives.
##' @param y numeric. L'echantillon des reponses binaires.
##' @param delta numeric. Parametre delta du modele logistique.
##' @param beta numeric. Parametre beta du modele logistique.
##'
##' @examples
##' sim <- simulation(100, 1.0, 2.0)
##' X <- as.numeric(unlist(sim[1]))
##' Y <- as.numeric(unlist(sim[2]))
##' hess_logit(X,Y, 1.0, 2.0)
##'
##' @return matrix.
##'
##'

hess_logit <- function(x,y,delta,beta){
  f_logit_x = f_logit(x,delta,beta)
  composante = (1 - f_logit_x) * f_logit_x
  d1 = sum(composante)
  d2 = sum(x* composante)
  d3 = sum(x**2 * composante)
  return ( matrix(c(-d1,-d2,-d2,-d3),nrow=2) )
}
