Data.Set = function(i.data,wd=getwd())
{
  #wd0=getwd()
  #setwd(wd)
  
  if(i.data==1){#6
    XY=read.csv(paste0(wd,"/35-servo.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  
  if(i.data==2){#6
    XY=read.csv(paste0(wd,"/3-autoMpg.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  
  if(i.data==3){
    data = read.table(paste0(wd,'/Concrete.data'))
    data = data.matrix(data)
    N = nrow(data)
    p = ncol(data)-1
    X0 = data[,1:p]
    y0 = data[,p+1] #N=1030,p=8
  }
  
  if(i.data==4){
    data = read.table(paste0(wd,'/housePrice.dat'), sep="")
    N = nrow(data)
    p = ncol(data)-1
    X0 = as.matrix(data[,1:p])
    y0 = log(as.matrix(data[,p+1])) #N=506,p=13
  }
  
  if(i.data==5){
    data=as.matrix(read.csv(paste0(wd,"/WildBlueberryPollinationSimulationData.csv")))
    X0 = data[,2:14]
    y0 = data[,18] #N=777,p=13
  }
  
  if(i.data==6){
    data = as.matrix(read.csv(paste0(wd,"/bodyfat.csv")))
    X0 = data[,2:15]
    y0 = data[,1]
    p = ncol(X0)
    N = length(y0) #N=252,p=14
  }
  
  if(i.data==7){
    data=as.matrix(read.csv(paste0(wd,"/ParisHousing.csv")))
    X0 = data[,1:16]
    y0 = data[,17] #N=10000,p=16
  }
  
  if(i.data==8){
    data = read.csv(paste0(wd,'/kc_house_data.csv'))
    N = nrow(data)
    p = ncol(data)
    X0 = as.matrix(data[,4:p])
    y0 = log(as.matrix(data[,3])) #N=21613,p=18
  }
  
  if(i.data==9){
    data = as.matrix(read.csv(paste0(wd,"/Bias_correction_ucl.csv")))
    p0 = ncol(data)
    data = data[,3:p0]
    p = dim(data)[2]
    XA = matrix(0, dim(data)[1], dim(data)[2])
    for (i in 1:p)
      XA[,i] = as.numeric(data[,i])
    n = nrow(XA)
    I = c()
    for (i in 1:n)
    {
      if (sum(is.na(XA[i,])) >0)
        I = c(I, i)
    }  
    XA = XA[-I,]
    N = nrow(XA)
    p = ncol(XA)
    X0 = XA[,1:(p-2)]
    y0 = XA[,p-1] #N=7590,p=21
  }
  
  if(i.data==10){#6
    XY=read.csv(paste0(wd,"/1-auto93.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }
  
  if(i.data==11){#6
    XY=read.csv(paste0(wd,"/2-autoHorse.csv"))
    X0=XY[,-ncol(XY)]
    y0=XY[,ncol(XY)]
  }

  if(i.data==12){
    XY = as.matrix(read.csv(paste0(wd,"/Facebook Comment Volume.csv"),header=F))
    XY = XY[XY[,ncol(XY)]>0,]
    aa = c()
    for(i in 1:ncol(XY)){
      aa = c(aa,length(unique(XY[,i])))
    }
    XY = XY[,aa>1]
    p = ncol(XY)-1 
    N = nrow(XY)
    X0 = XY[,1:p]
    y0 = XY[,p+1] #p=52,N=18370
  }
  
  if(i.data==13){
    data = read.csv(paste0(wd,"/FINAL_USO.csv"))
    X0 = as.matrix(data[,8:81])
    y0 = as.matrix(data[,6]) #N=1718,p=74
  }
  
  if(i.data==14){
    XY = read.csv(paste0(wd,"/Baseball_player_statistics/Baseball_player_statistics.csv"))
    y0 = XY[,12]/XY[,10]  
    X0 = as.matrix(XY[,-c(1,3:7,10,12)])
    p = ncol(X0) 
    N = length(y0) #p=74,N=4535
  }
  
  if(i.data==15){
    XY = read.csv(paste0(wd,"/Processed_DJI.csv"))[,-c(1,3,59)]
    aa = c()
    for(i in 1:ncol(XY)){aa=c(aa,sum(is.na(XY[,i])))}
    XY = XY[,aa<400]
    bb = c()
    for(j in 1:nrow(XY)){bb=c(bb,sum(is.na(XY[j,])))}
    XY = XY[bb==0,]
    y0 = XY[,1]
    X0 = as.matrix(XY[,-1])
    p = ncol(X0)
    N = length(y0) #p=76,N=1441
  }
  
  if(i.data==16){
    data = read.csv(paste0(wd,'/superconduct_train.csv'))
    p = ncol(data)
    N = nrow(data)
    X0 = as.matrix(data[,1:p-1]);
    y0 = as.matrix(data[,p]);
    I = (apply(X0, 2, sd)>0)
    X0 = X0[,I]
    p = ncol(X0) #p=81,N=21263
  } 
  
  if(i.data==17){
    XY = as.matrix(read.csv(paste0(wd,'/Warsaw.csv')))
    p = ncol(XY)-1 
    N = nrow(XY)
    X0 = XY[,1:p]
    y0 = XY[,p+1] #p=84,N=3472
  }
  
  if(i.data==18){
    XY = as.matrix(read.table(paste0(wd,'/TomsHardware.data'),sep=","))
    p = ncol(XY)-1 
    N = nrow(XY)
    X0 = XY[,1:p]
    y0 = XY[,p+1] #N=28179,p=96
  }
  
  if(i.data==19){
    data = read.table(paste0(wd,'/communities.data'), sep=",")
    p = ncol(data)
    I = c(4)
    for (i in 1:p){
      if (mean(data[,i]=="?") >0) I = c(I, i) 
    }
    data = as.matrix(data[,-I])
    N = nrow(data)
    p = ncol(data)-1
    X0 = data[,1:p]
    y0 = data[,p+1] #N=1994,p=101
  }
  
  if(i.data==20){
    XY = as.matrix(read.csv(paste0(wd,'/Residential_Building_Data_Set.csv'),header=F))  
    X0 = XY[,1:103]
    y = XY[, 104] # sales price
    N = length(y)
    p = ncol(X0)
    y0 = log(y) #N=372,p=103
  }
  
  ##################################################
  #n = dim(X0)[1]
  #p = dim(X0)[2]
  
  #I = union(c(which(rowSums(is.na(X0))>0),which(rowSums(X0==0)==p)),which(is.na(y0)))
  I = which((rowSums(is.na(X0))>0)|is.na(y0))#,which(rowSums(X==0)==p))
  
  if(length(I)>0){
    X0=X0[-I,]
    y0=y0[-I]
  }
  
  return(list(X = X0, y=y0))
}

