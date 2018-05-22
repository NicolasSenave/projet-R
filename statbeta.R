###################################################################################
##'
##'
##' Retourne la p-valeur associee au test de nullite du coefficient beta
##' ainsi que l'intervalle de confiance de beta au niveau 95 pourcents.
##'
##' @param x numeric.
##' @param y numeric.
##' @param delta numeric.
##' @param beta numeric.
##' @param alpha numeric.
##'
##' @examples
##' sim <- simulation(100, 1.0, 0.0)
##' X <- as.numeric(unlist(sim[1]))
##' Y <- as.numeric(unlist(sim[2]))
##' estimation(X,Y)
##' statbeta(X,Y, 1.0)
##'
##' @return numeric. Vecteur (p-valeur, borne_inf, borne_sup)
##' @export
##'
##'


statbeta <- function(x,y, delta,beta, alpha=0.05){
  d3<-sum(x**2*composante)
  z=(delta+beta*x)
  composante<-(1/(1+exp(z)))*(1-1/(1+exp(z)))
  betachap=newtonraphson(x,y)[2,]
  sigmachap=(d3**-1)**0.5
  if (betachap/sigmachap>0){
    p_value<-2*(1-pt(betachap/sigmachap,n-1))
  }
  else{
    p_value<-2*pt(betachap/sigmachap,n-1)
  }
  n=length(x)
  borneinf<-betachap-qt((1-alpha/2),n-1)*sigmachap
  bornesup<-betachap+qt((1-alpha/2),n-1)*sigmachap
  res<- c(p_val,borneinf,bornesup)
  return(res)
}
