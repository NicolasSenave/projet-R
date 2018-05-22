###################################################################################
##'
##'
##' Inverse une fonction representee sous forme de matrice.
##' La matrice doit etre de taille 2x2 et inversible
##'
##' @param matrice matrix. Matrice 2x2 de la fonction.
##'
##' @examples
##' mat = matrix(c(2,0,0,3), nrow=2,ncol=2) 
##' inversefonction(mat)
##'
##' @return matrix. Matrice inversee 2x2.
##'
##'


inverse_fonction <- function(matrice){
  det = matrice[1]*matrice[4] - matrice[2]*matrice[3]
  mat1 = matrix(c(matrice[4],-matrice[2],-matrice[3],matrice[1]),nrow=2)
  return( 1/det * mat1 )
}