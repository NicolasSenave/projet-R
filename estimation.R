###################################################################################
##'
##'
##' Estime les parametres (delta, beta) du maximum de vraisemblance 
##' du modele logistique pour un echantillon, par un algorithme de Newton-Raphson.
##'
##' @param echantillon numeric. L'echantillon dont on veut estimer les parametres.
##'
##' @examples
##' sim <- simulation(100, 1.0, 2.0)
##' echantillon <- as.numeric(unlist(sim[2]))
##' estimation(echantillon)
##'
##' @return numeric. Vecteur (delta,beta).
##' @export
##'
##'


estimation <- function(echantillon){
  delta <- 0.0
  beta <- 0.0
  # ...
  return(c(delta,beta))
}