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



newtonraphson <- function(x,y){
  delta <- 0.1
  beta <- 0.5
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
