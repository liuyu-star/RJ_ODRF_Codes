library(earth)
library(MAVE)

DX=function(X,coef=coef){
  X=as.matrix(X)
  np=dim(X);n=np[1];p=np[2]
  
  h=function(x){ifelse(x>0,x,0)}
  
  Xp=c(paste0("X",seq(p)))
  for (ip in seq(p)) {
    assign(Xp[ip],X[,ip])
  }
  
  dX=matrix(0,n,p)
  for (i in 2:length(coef)) {
    hi=unlist(strsplit(rownames(coef)[i],split = ""))
    if("*"%in%hi){
      hi=unlist(strsplit(rownames(coef)[i],split = "*",fixed = T))
    }else{
      hi=rownames(coef)[i]
    }
    
    xn=matrix(0,n,p)
    dhx=rep(list(xn),length(hi))
    
    for (j in seq(length(hi))) {
      hj=unlist(strsplit(hi[j],split = ""))
      Xid=which("X"==hj)
      
      if("h"%in%hj){
        nk=ifelse(hj[Xid-1]=="-",nchar(hi[j]),min(which("-"==hj)))-1
        xj=eval(parse(text = substr(hi[j],3,nchar(hi[j])-1)))
        hxjk=ifelse("-"==hj[Xid-1],-1,1)*(xj>0)
      }else{
        nk=nchar(hi[j])
        hxjk=rep(1,n)
      }
      k=as.numeric(substr(hi[j],Xid+1,nk))
      xn[,k]=1
      hxj=eval(parse(text = hi[j]))
      
      dhx[[j]]=matrix(hxj,n,p)
      dhx[[j]][,k]= hxjk
    }
    
    dXij=matrix(1,n,p)
    for (j in seq(length(dhx))) {
      dXij=dXij*dhx[[j]]
    }
    dXij=dXij*xn
    dX=dX+coef[i]*dXij
  }
  
  return(dX)
}

OPG=function(X,y,degree = ncol(X)){
  #x=sort(runif(100)) 
  #y=sin(2*pi*x)
  #X=matrix(rnorm(300),100,3)
  #y=(X[,1]+X[,2]+0*X[,3])^2+rnorm(100)
  
  X=as.matrix(X)
  A=earth(y~X,degree = degree)
  coef=A$coefficients
  
  d=DX(X,coef=coef)
  #d=t(apply(X, 1,Dx,coef=coef))
  
  #n=nrow(X)
  #p=ncol(X)
  #out=matrix(0,p,p)
  #for (i in seq(n)) {
  #  out=out+d[i,]%o%d[i,]
  #}
  out=t(d)%*%d
  #theta=eigen(out)$vectors[,1]
  
  return(list(theta=eigen(out)$vectors,gcv=A$gcv, S = out)) 
}

OPG.rep=function(X,y,dm = 1,ntheta.max=10, df=2){
  X=as.matrix(X)
  p = dim(X)[2]
  
#  gcv=sapply(seq(min(p,10)),function(df){earth(y~X, degree=df)$gcv})
#  df=which.min(gcv)
  
  ytheta1=OPG(X,y,degree = df)
  gcv=ytheta1$gcv
  theta0 = as.matrix(ytheta1$theta[,1:dm])
  theta1 = theta0
  
  esp=0;ntheta=1
  while ((ntheta<=ntheta.max)&(esp<=1e-04)) 
    {
    XT=as.matrix(cbind(X%*%theta1, X)) 
    ytheta2=OPG(XT,y,degree = df)
    if(ytheta2$gcv<gcv){
      esp=abs(ytheta2$gcv-gcv)
      gcv=ytheta2$gcv
      
      theta2=as.matrix(ytheta2$theta[,1:dm])
      theta1=theta1%*%theta2[1:dm,]+theta2[-(1:dm),]
      theta1 = theta1/matrix(sqrt(colSums(theta1^2)), p, dm, byrow=TRUE)
    }else{
      break
    }
    ntheta=ntheta+1
  }
  
  return(list(theta0=as.matrix(theta0),theta1=as.matrix(theta1),ntheta=ntheta-1,degree = df))
}  

OPG.slice <- function(X, y, H)
{
  X=as.matrix(X)
  n = nrow(X)
  S = 0
  K = ceiling( n/H)
  for (h in 1:H)
  {
    I = ((h-1)*K+1):min(n, h*K)
    S = S + OPG(X[I,], y[I], degree=1)$S
  }
  eig = eigen(S)
  
  return(list(vectors = eig$vectors))
}
