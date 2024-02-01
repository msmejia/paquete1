#pasar a una matriz los n primeros numeros compuestos

es_compuesto <- function(Num){
  i=1
  cdiv=0
  while(i<=Num){
    if(Num %% i == 0){
      cdiv<-cdiv+1
    }
    i=i+1
  }
  if(cdiv>2){
    p<-TRUE
  }else{
    p<-FALSE
  }
  return(p)
}

#funcion que permite calcular los N primeros compuestos
N_compuestos_matriz <- function(Nf,Nc){
  A<-matrix(data = NA,nrow = Nf,ncol = Nc,byrow = FALSE)
  Num<-1
  cont<-1
  B<-c()
  N<-Nf*Nc
  while(cont<=N){
    Resp<-es_compuesto(Num)
    if(Resp==TRUE){
      B<-c(B,Num)
      cont<-cont+1
    }
    Num<-Num+1
  }
  k<-1
  for(i in 1:Nf){
    for(j in 1:Nc){
        A[i,j]=B[k]
        k<-k+1
      }
  }
  return(A)
}
