###################################################################################
##'
##'
##' Renvoie le gradient de la log-varisemblance
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
##' grad_logit(X,Y, 1.0, 2.0)
##'
##' @return matrix.
##'
##'


grad_logit <- function(x,y,delta,beta){
  p = f_logit(x,delta,beta)
  composante_x = y - p
  x1 = sum(composante_x)
  y1 = sum(x * composante_x)
  return( matrix(c(x1,y1),nrow=2) )
}
