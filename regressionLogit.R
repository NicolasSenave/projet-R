##' Regression logistique
##'
##' Ce package permet d'effectuer une regression logistique pour estimer
##' les parametres d'un jeu de donnees X explicatives continues et Y d'interet binaires.
##'
##'
##' \tabular{ll}{
##'   Package: \tab regressionLogit\cr
##'   Type: \tab Package\cr
##'   Version: \tab 1.0.0\cr
##'   Date: \tab 2018-05-22\cr
##'   License: \tab GPL-2\cr
##'   LazyLoad: \tab yes\cr
##' }
##'
##'
##' @name regressionLogit-package
##' @aliases regressionLogit
##' @rdname regressionLogit-package
##' @docType package
##' @keywords package
##' @author
##' Quentin NOUVELLON, Killian Poulain, Nicolas SENAVE
##'
##'
##' @examples
##' X,Y <- "vos donnees"
##'
##' param <- estimation(X,Y)
##' beta_estime <- param[1]
##' delta_estime <- param[2]
##'
##' visualisation(X,Y)
##'
##'
NULL
