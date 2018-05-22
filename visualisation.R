###################################################################################
##'
##'
##' Represente graphiquement la distribution de Y conditionnellement a X = x 
##' en fonction de x ou X est un echantillon de valeurs explicatives
##' et ou Y est un echantillon d'interet binaires.
##'
##' @param X numeric. Un echantillon de donnees explicatives.
##' @param Y numeric. Un echantillon de donnees binaires.
##' @param nb_col numeric. Par défaut nb_col=10. Nombre de colonnes de l'histogramme.
##'
##' @examples
##' sim <- simulation(n,delta,beta)
##' X <- as.numeric(unlist(sim[1]))
##' Y <- as.numeric(unlist(sim[2]))
##' visualisation(X,Y)
##'
##' @return NULL. Affiche la représentation graphique.
##' @export
##'
##'


visualisation <- function(X,Y,nb_col=10){
  
  # Affiche le graphe avec la courbe theorique et les y
  
  # Points de l'echantillon y
  plot(X,Y,
       main = "Courbe theorique et points de l'echantillon binaire Y",
       xlab = "Valeurs x de l'echantillon X",
       ylab = "")
  legend("right",
         legend = "Valeurs de Y|X=x dans la simulation",
         pch=1, cex=0.7)
  
  # Courbe theorique
  abscisses <- seq(min(X), max(X), 0.01)
  lines(abscisses, f_logit(abscisses,delta,beta),
        col = "red")
  legend("bottomright",
         legend = "E(Y|X=x)",
         col = "red", lty=1:0, cex=0.7)
  
  # Affichage de l'histogramme des la répartition des y
  
  n = length(X)
  distribution_y <- c()
  k = 0
  for (i in (1:n)){
    if (Y[i] == 1){
      distribution_y[k] <- X[i]
      k = k+1
    }
  }
  hist(distribution_y,
       breaks = nb_col,
       main="Distribution de Y conditionnellement a X=x",
       xlab="Valeurs x de l'echantillon X",
       ylab="Card(Y = 1|X=x)")
  
}