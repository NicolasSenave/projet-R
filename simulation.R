###################################################################################
##'
##'
##' Genere n realisations independantes (x1,y1), ..., (xn,yn)
##' du couple (X,Y) ou X suit une loi normale centree reduite
##' et ou Y|X = x suit un modele logit de parametres (delta,beta).
##'
##' @param n numeric. Le nombre de realisations.
##' @param delta numeric. Parametre delta du modele logistique.
##' @param beta numeric. Parametre beta du modele logistique.
##'
##' @examples
##' simulation(100, 1.0, 2.0)
##'
##' @return list. Contient les vecteurs X et Y de logueur n.
##' @export
##'
##'


simulation <- function(n,delta,beta){
  X <- c(rnorm(n))
  Y <- c(as.numeric(runif(n) < f_logit(X,delta,beta)))

  return(list(X,Y))
}
