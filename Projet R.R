# Projet statistique avec R
# Sujet 5 : regression logit
# 
# Quentin NOUVELLON, Kilian POULAIN, Nicolas SENAVE


## Parametres

n <- 100

val_delta <- 1.0
val_beta <- 2.0


## Fonctions mathematiques

f_logit <- function(x, delta=val_delta,beta=val_beta){
  exp(delta+x*beta) / (1 + exp(delta+x*beta))
}

densite_logit <- function(x, delta=val_delta,beta=val_beta){
  beta * exp(delta+x*beta) / (1 + exp(delta+x*beta))^2
}

grad_logit <- function(x, delta=val_delta,beta=val_beta){
  res <- c(0,0)
}


## Fonctions du package

simulation <- function(){
  # Genere n realisations independantes (x1,y1), ..., (xn,yn) 
  # du couple (X,Y) ou X suit une loi normale centree reduite 
  # et ou Y|X = x suit un modele logit de parametres (delta,beta).
  x <- c(rnorm(n))
  Y <- c(1:100)
  for (i in (1:n)){
    if (runif(1) < f_logit(x[i])){
      Y[i] = 1
    }
    else{
      Y[i] = 0
    }
  }
  return(list(x,Y))
}

visualisation <- function(){
  # Represente graphiquement la distribution de Y conditionnellement a X = x 
  # en fonction de x pour des valeurs donnees de delta et beta. C'est une bernoulli. Barplot
  x <- runif(1000,-3,3)
  plot(x,densite_logit(x),type=('p'))
}

visualisation1<-function(n,delta,beta,x){
  prob <- f_logit(x,delta,beta)
  #Ybis <- runif(n,0,1)
  #Ytierce <- Ybis < prob
  #Y <- vapply(Ytierce, convbool,1)
  #Ylol <- c(sum(Y==1),sum(Y==0))
  #pie(Ylol)
  P <- c(prob, 1-prob)
  pie(P,main="Camembert de la r?partiton th?orique de Y",labels=c(1,0), col=c("blue","orange"))
}


  
probabilite<-function(x,delta,beta){
  z=(delta+beta*x)
  return(1-1/(1+exp(z)))
}

log_vraisemblance<-function(x,y,delta,beta){
  z<-(delta+beta*x)
  z1<-z*y-log((1+exp(z)))
  return(sum(z1))
}

gradient<-function(x,y,delta,beta){
  p<-probabilite(x,delta,beta)
  composantex<-y-p
  x1<-sum(composantex)
  y1<-sum(x*composantex)
  return(matrix(c(x1,y1),nrow=2))
}

hessienne<-function(x,y,delta,beta){
  z=(delta+beta*x)
  composante<-(1/(1+exp(z)))*(1-1/(1+exp(z)))
  d1<-sum(composante)
  d2<-sum(x*composante)
  d3<-sum(x**2*composante)
  return (matrix(c(-d1,-d2,-d2,-d3),nrow=2))
  
}

norme<-function(x){
  return (t(x)%*%x)
}






newtonraphson<-function(x,y){
  delta<- 0.1
  beta<- 0.5
  i=0
  v0<-matrix(c(delta,beta),nrow=2)
  v1<-v0-solve(hessienne(x,y,v0[1],v0[2]))%*%gradient(x,y,v0[1],v0[2])
  grad1<-gradient(x,y,v1[1],v1[2])
  grad2<-gradient(x,y,v0[1],v0[2])
  while(norme(grad1-grad2)>0.002&i<3000){
    v0<-v1
    v1<-v0-solve(hessienne(x,y,v0[1],v0[2]))%*%gradient(x,y,v0[1],v0[2])
    grad1<-gradient(x,y,v1[1],v1[2])
    grad2<-gradient(x,y,v0[1],v0[2])
    i=i+1
  }
  return(v1)             
}


newtonraphson(x,y)



    
  
  

  
 

prediction <- function(echantillon,delta,beta){
  # Prend comme arguments n realisations independantes (x1,...,xn) de X 
  # ainsi que des parametres (delta,beta). 
  # Cette fonction retourne P(Y = 1|X = xi) pour chaque xi 
  # ainsi qu'une valeur predite yhat_i.
  proba <- vector("numeric",n)
  yhat_i <- 0.0
  
  return(proba,yhat_i)
}

prediction1<-function(x){
  P<-p(x,delta,beta)
  Y<-round(P,0)
  return(rbind(P,Y))
}

statbeta <- function(){
  # Retourne la p-valeur associee au test de nullite du coefficient beta 
  # ainsi que l'intervalle de confiance de beta au niveau 95%.
  p_val <- 0.0
  IC_lower <- 0.0
  IC_upper <- 0.0
  
  return(p_val,IC_lower,IC_upper)
}
