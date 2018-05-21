###################################################################################
##'
##'
##' Represente graphiquement la distribution de Y conditionnellement a X = x 
##' en fonction de x ou X est un echantillon de valeurs
##' et ou Y|X = x suit un modele logit de parametres (delta,beta).
##'
##' @param echantillon numeric. Un echantillon de données.
##' @param delta numeric. Parametre delta du modele logistique.
##' @param beta numeric. Parametre beta du modele logistique.
##' @param nb_col numeric. Par défaut nb_col=10. Nombre de colonnes de l'histogramme.
##'
##' @examples
##' echantillon <- c(rnorm(100))
##' visualisation(echantillon, 1.0, 2.0)
##'
##' @return NULL. Affiche la représentation graphique.
##' @export
##'
##'


visualisation <- function(echantillon,delta,beta,nb_col=10){
  hist(f_logit(echantillon,delta,beta),
       breaks = nb_col,
       main="Distribution de Y conditionnellement à X=x",
       xlab="Valeurs x de l'echantillon X",
       ylab="Card(Y = 1|X=x)")
}