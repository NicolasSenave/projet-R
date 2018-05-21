###################################################################################
##'
##'
##' Prend comme arguments un echantillon de n realisations independantes 
##' (x1,...,xn) d'un echantillon ainsi que des parametres (delta,beta) 
##' du modèle logit.
##' Cette fonction retourne deux vecteurs P et Yhat de longueur n :
##' - P contenant les P(Y = 1|X = xi) pour chaque xi ;
##' - Yhat contenant les valeurs predites de Y ;
##' ou Y|X = xi suit un modele logit de parametres (delta,beta).
##'
##' @param echantillon numeric. Un echantillon de données explicatives.
##' @param delta numeric. Parametre delta du modele logistique.
##' @param beta numeric. Parametre beta du modele logistique.
##'
##' @examples
##' echantillon <- runif(100)
##' prediction(echantillon, 1.0, 2.0)
##'
##' @return list. Contient les vecteurs P et Yhat.
##' @export
##'
##'


prediction <- function(echantillon,delta,beta){
  P <- f_logit(echantillon,delta,beta)
  n = length(echantillon)
  Yhat <- c(as.numeric(runif(n) < P))
  return(list(P,Yhat))
}