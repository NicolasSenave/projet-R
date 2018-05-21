###################################################################################
##'
##'
##' Estime les parametres (delta, beta) du maximum de vraisemblance 
##' du modele logistique pour un couple de donnees 
##' X (donnees explicatives) et Y (reponses binaires), 
##' par un algorithme de Newton-Raphson.
##'
##' @param X numeric. L'echantillon des donnees explicatives.
##' @param Y numeric. L'echantillon des reponses binaires.
##'
##' @examples
##' sim <- simulation(100, 1.0, 2.0)
##' X <- as.numeric(unlist(sim[1]))
##' Y <- as.numeric(unlist(sim[2]))
##' estimation(X,Y)
##'
##' @return numeric. Vecteur (delta,beta).
##' @export
##'
##'


estimation <- function(X, Y){
  delta <- 0.0
  beta <- 0.0
  # ...
  return(c(delta,beta))
}