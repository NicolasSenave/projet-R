###################################################################################
##'
##'
##' Estime les parametres (delta, beta) du maximum de vraisemblance
##' du modele logistique pour un couple de donnees
##' X (donnees explicatives) et Y (reponses binaires),
##' par un algorithme de Newton-Raphson.
##'
##' @param x numeric. L'echantillon des donnees explicatives.
##' @param y numeric. L'echantillon des reponses binaires.
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


estimation <- function(x,y){
  delta = 0
  beta = 0
  v0 = matrix(c(delta,beta),nrow=2)
  v1 = v0 - inverse_fonction(hess_logit(x,y,v0[1],v0[2]))%*%grad_logit(x,y,v0[1],v0[2])
  grad1 = grad_logit(x,y,v1[1],v1[2])
  grad2 = grad_logit(x,y,v0[1],v0[2])
  i = 0
  while( norme(grad1-grad2)>10**-20 & i<30000 ){
    v0 = v1
    v1 = v0-inverse_fonction(hess_logit(x,y,v0[1],v0[2]))%*%grad_logit(x,y,v0[1],v0[2])
    grad1 = grad_logit(x,y,v1[1],v1[2])
    grad2 = grad_logit(x,y,v0[1],v0[2])
    i = i+1
  }
  delta = v1[1]
  beta = v1[2]
  return(c(delta,beta))
}
