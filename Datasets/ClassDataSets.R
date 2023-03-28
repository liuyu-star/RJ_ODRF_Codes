Data.Set = function(i.data,wd=getwd())
{
  #class=3
  if(i.data==1){
    XY=read.csv(paste0(wd,"/iris.data"))
    X0=XY[,-5]
    y0 = XY[,5]
    #149   p=4
  }
  
  #class=3
  if(i.data==2){
    XY=read.csv(paste0(wd,"/16-seeds.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)] #p=7
  }
  
  #class=3
  if(i.data==3){
    XY=read.csv(paste0(wd,"/milknew.csv"))
    X0 = XY[,-8]
    y0 = XY[,8]
    #1059    p=7
  }
  
  #class=6
  if(i.data==4){
    XY=read.csv(paste0(wd,"/4-breastTissue.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]#p=9
  }
  
  #class=3
  if(i.data==5){
    XY = read.csv(paste0(wd,'/penguins_size.csv'))[-337,]
    XY=XY[which(complete.cases(XY)),]
    X = XY[,-1]
    y0 = XY[,1]
    n=length(y0)
    Xcat=c(1,6)
    numCat <- apply(X[,Xcat,drop = FALSE], 2, function(x) length(unique(x)))
    X1 <- matrix(0, nrow = n, ncol = sum(numCat))
    col.idx <- 0L
    for (j in 1:length(Xcat)) {
      cat.map <- (col.idx + 1L):(col.idx + numCat[j])
      # convert categorical feature to K dummy variables.
      X1[, cat.map] <- (matrix(X[,Xcat[j]],n,numCat[j])==matrix(unique(X[,Xcat[j]]),n,numCat[j],byrow = TRUE))+0
      col.idx <- col.idx + numCat[j]
    }
    X0=cbind(X1,X[,-Xcat]) 
    #333   p=9
  }
  
  #class=3
  if(i.data==6){
    XY=read.csv(paste0(wd,"/wine.data"))
    X0=XY[,-1]
    y0 = XY[,1]
    #177  p=13
  }
  
  #class=4
  if(i.data==7){
    XY=read.csv(paste0(wd,"/Mobile Price Classification/train.csv"))
    #XY =rbind(XY,read.csv("Mobile Price Classification/test.csv"))
    X0 = XY[,-21]
    y0 = XY[,21]
    #2000   p=20
  }
  
  #class=3
  if(i.data==8){
    XY=read.csv(paste0(wd,"/23-waveform.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]#p=21
  }
  
 #class=2  
 ############################################################################### 
  if(i.data ==9){
    yx=read.csv(paste0(wd,"/data21.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data==10){
    yx=read.table(paste0(wd,"/data16.txt"),sep=",")
    p=ncol(yx)-1
    n=nrow(yx) #n=19020,p=10
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data==11){
    yx=read.csv(paste0(wd,"/data19.csv"))
    n=nrow(yx)
    p=ncol(yx)-1 #n=270,p=13
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  
  if(i.data==12){
    yx = as.matrix(read.table(paste0(wd,"/data1.txt"),sep=","))
    p = ncol(yx)-1
    n = nrow(yx) #n=14980,p=14
    X0 = yx[,1:p]
    y0 = yx[,p+1]
  }
  
  if(i.data ==13){
    yx=read.csv(paste0(wd,"/data24.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data==14){
    yx=read.table(paste0(wd,"/data14.txt"),sep=",")
    p=ncol(yx)-1
    n=nrow(yx) #n=1151,p=19
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data ==15){
    yx=read.csv(paste0(wd,"/data26.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data==16){
    yx=read.csv(paste0(wd,"/data9/Pistachio_28_Features_Dataset.csv"))
    p=ncol(yx)-1
    n=nrow(yx) #n=2148,p=28
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data==17){
    yx=read.csv(paste0(wd,"/data7.csv"))
    y0=as.integer(as.factor(yx[,2]))
    y0[y0==2]=0
    X0=as.matrix(yx[,3:32])
    n=length(y0)
    p=ncol(X0) #n=569,p=30
  }
  
  if(i.data==18){
    yx=read.table(paste0(wd,"/data12.data"),sep=",")
    p=ncol(yx)-1
    n=nrow(yx) #n=351,p=34
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  
  if(i.data==19){
    yx=read.csv(paste0(wd,"/data13.csv"),header=F)
    p=ncol(yx)-1
    n=nrow(yx) #n=1055,p=41
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
    X0=as.matrix(yx[,1:p])
  }
  
  #if(i.data ==20){
  #  yx=read.csv(paste0(wd,"/data27.csv"))
  #  p=ncol(yx)-1
  #  n=nrow(yx)
  #  y0=yx[,p+1]
  #  X0=as.matrix(yx[,1:p])
  #}
  
  if(i.data==20){
    yx=read.table(paste0(wd,"/data2.csv"),header=TRUE,sep=",")[,-c(1,80:82)]
    del=order(as.vector(apply(is.na(yx[,1:77]),2,sum)),decreasing=TRUE)[1:6]
    yx=yx[,-del]
    yx=yx[apply(is.na(yx),1,sum)==0,]
    yx=yx[,-69] #69th column of data set 2 is the same as 54th column
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=1047,p=70
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  
  if(i.data ==21){
    yx=read.csv(paste0(wd,"/data22.csv"))
    p=ncol(yx)-1
    n=nrow(yx)
    y0=yx[,p+1]
    X0=as.matrix(yx[,1:p])
  }
  #if(i.data==22){
  #  yx=read.csv(paste0(wd,"/data8.csv"))
  #  y0=yx[,1]
  #  X0=as.matrix(yx[,2:96])
  #  n=length(y0)
  #  p=ncol(X0) #n=6819,p=95
  #}
  
  #if(i.data==23){#5
  #  XY=read.csv(paste0(wd,"/6-hillValleyNoisy.csv"))#5-hillValley.csv
  #  X0=XY[,-ncol(XY)]
  #  y0=XY[,ncol(XY)]
  #}
  
  if(i.data==22){
    yx1=read.table(paste0(wd,"/data3/Hill_Valley_without_noise_Testing.txt"),header=TRUE,sep=",")
    yx2=read.table(paste0(wd,"/data3/Hill_Valley_without_noise_Training.txt"),header=TRUE,sep=",")
    yx=rbind(yx1,yx2)
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=1212,p=100
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  
  if(i.data==23){
    yx=read.table(paste0(wd,"/data4/clean2.txt"),header=FALSE,sep=",")[,-(1:2)]
    n=dim(yx)[1]
    p=dim(yx)[2]-1 #n=6598,p=166
    yx[,p+1]=as.integer(as.factor(yx[,p+1]))
    X0=as.matrix(yx[,1:p])
    y0=rep(0,n)
    y0[yx[,p+1]==2]=1
  }
  
  #if(i.data==25){
  #  yx1=as.matrix(read.csv(paste0(wd,"/data11/ptbdb_normal.csv")))
  #  yx2=as.matrix(read.csv(paste0(wd,"/data11/ptbdb_abnormal.csv")))
  #  yx=rbind(yx1,yx2)
  #  yx=yx[,-187]
  #  p=ncol(yx)-1
  #  n=nrow(yx) #n=14550,p=196
  #  y0=yx[,p+1]
  #  X0=as.matrix(yx[,1:p])
  #}
  
  ##################################################
  #n = dim(X0)[1]
  #p = dim(X0)[2]
  
  #I = union(c(which(rowSums(is.na(X0))>0),which(rowSums(X0==0)==p)),which(is.na(y0)))
  I = which((rowSums(is.na(X0))>0)|is.na(y0))#,which(rowSums(X==0)==p))
  
  if(length(I)>0){
    X0=X0[-I,]
    y0=y0[-I]
  }
  
  
  return(list(X = X0, y=as.factor(y0)))
}

