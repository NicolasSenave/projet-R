###################################################################################
##'
##'
##' Represente graphiquement la distribution de Y conditionnellement a X = x
##' en fonction de x ou X est un echantillon de valeurs explicatives
##' et ou Y est un echantillon d'interet binaires.
##'
##' @param X numeric. Un echantillon de donnees explicatives.
##' @param Y numeric. Un echantillon de donnees binaires.
##' @param param numeric. c(delta,beta). Par defaut param=c().
##' @param nb_col numeric. Par défaut nb_col=10. Nombre de colonnes de l'histogramme.
##'
##' @examples
##' sim <- simulation(100, 1.0, 2.0)
##' X <- as.numeric(unlist(sim[1]))
##' Y <- as.numeric(unlist(sim[2]))
##' visualisation(X,Y, c(1.0,2.0) )
##'
##' @return NULL. Affiche la représentation graphique.
##' @export
##'
##'


visualisation <- function(X,Y,param=NULL,nb_col=10){

  plot(X,Y,
       main = "Courbe theorique et points de l'echantillon binaire Y",
       xlab = "Valeurs x de l'echantillon X",
       ylab = "")
  legend("right",
         legend = "Valeurs de Y|X=x dans la simulation",
         pch=1, cex=0.7)

  if (length(param) == 2){
    abscisses <- seq(min(X), max(X), 0.01)
    lines(abscisses, f_logit(abscisses,param[1],param[2]),
          col = "red")
    legend("bottomright",
           legend = "E(Y|X=x)",
           col = "red", lty=1:0, cex=0.7)
  }

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
