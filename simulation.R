simulation <- function(n,delta,beta){
  # Genere n realisations ind??pendantes (x1,y1), ..., (xn,yn)
  # du couple (X,Y) ou X suit une loi normale centree reduite
  # et ou Y|X = x suit un modele logit de parametres (delta,beta).
  x <- c(rnorm(n))
  Y <- c(1:100)
  for (i in (1:n)){
    if (runif(1) < f_logit(x[i],delta,beta)){
      Y[i] = 1
    }
    else{
      Y[i] = 0
    }
  }
  return(list(x,Y))
}
