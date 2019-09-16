#Rootman Group 1 R code


HalfEyeLid<-function(x,trs=c(0.0017,0)) {
  clean<-trs[1]
  throw<-trs[2]
  x<-as.matrix(x)
  for (i in 1:nrow(x)) {
    parti<-matrix(numeric(0),ncol=2)
    for (j in seq(1,ncol(x),by=2)) {
      if (is.na(x[i,j])==FALSE){
        parti<-rbind(parti,c(as.numeric(x[i,j]),as.numeric(x[i,j+1])))
      }
    }
    parti<-cbind(parti,rep(NA,nrow(parti)))
    parti[,3]<-parti[,2]
    parti[,2]<-(parti[,1])^2
    colnames(parti)<-c("X","X^2","Y")
    parti<-as.data.frame(parti)
    model_parti<-lm(Y~X+`X^2`,data=parti)
    if (is.na(model_parti$coefficients[3])) {
      parti<-parti
    }else if (model_parti$coefficients[3]<clean&model_parti$coefficients[3]>throw) {
      index<-which(parti$Y<=predict(model_parti))
      parti<-parti[index,]
    }else if (model_parti$coefficients[3]<=throw) {
      parti<-NULL
    }
    cleaned_new<-rep(NA,ncol(x))
    if (!is.null(parti)) {
      cleaned_new[seq(1,2*(length(parti[,1]))-1,by=2)]<-parti[,1]
      cleaned_new[seq(2,2*(length(parti[,1])),by=2)]<-parti[,3]
    }
    x[i,]<-cleaned_new
  }
  return(x)
}


##### A Data Wrapping Function: Enhancement In outlier removal
WrapMatrix<-function(x) {
  df<-data.frame("X"=numeric(0),"X^2"=numeric(0),"Y"=numeric(0))
  for (i in 1:nrow(x)) {
    parti<-matrix(numeric(0),ncol=2)
    for (j in seq(1,ncol(x),by= 2)) {
      if (is.na(x[i,j])==FALSE) {
        parti<-rbind(parti,c(as.numeric(x[i,j]),as.numeric(x[i,j+1])))
      }
    }
    parti<-cbind(parti,rep(NA,nrow(parti)))
    parti[,3]<-parti[,2]
    parti[,2]<-(parti[,1])^2
    colnames(parti)<-c("X","X^2","Y")
    parti<-as.data.frame(parti)
    model_parti<-lm(Y~X+`X^2`,data=parti)
    if (model_parti$coefficients[3]<0.004&model_parti$coefficients[3]>0.0000) {
      index<-which(parti$Y<=predict(model_parti))
      parti<-parti[index,]
    }else if (model_parti$coefficients[3]<=0.000) {
      parti<-NULL
    }
    df<-rbind(df,parti)
  }
  return(df[order(df[, 1]), ])
}


##### A Data Wrapping Function: Enhancement In outlier removal
WrapMatrixOld<-function(x) {
  df<-data.frame("X"=numeric(0),"X^2"=numeric(0),"Y"=numeric(0))
  for (i in 1:nrow(x)) {
    parti<-matrix(numeric(0),ncol=2)
    for (j in seq(1,ncol(x),2)) {
      if (is.na(x[i,j])==FALSE) {
        parti<-rbind(parti,data.frame("X"=as.numeric(x[i,j]),
                                      "X^2"=as.numeric(x[i,j])^2,
                                      "Y"=as.numeric(x[i,j+1])))
      }
    }
    df<-rbind(df,parti)
  }
  df<-df[order(df[,1]),]
  colnames(df)<-c("X","X^2","Y")
  return(df)
}


### Outliers Removal Function: 15 by default in a group
OutlierRemovalXY<-function(x,gap=15) {## x is a n*2 matrix 
  x<-x[order(x[, 1]), ]
  n<-nrow(x)
  ### Decides how many blocks are needed for outlier removal
  Blocks<-n%/%gap
  if (Blocks==0) {
    Blocks<-1
  }
  Remaining<-n%%gap
  ### Outlier Identifying
  newmatrix<-matrix(numeric(0),ncol=3)
  for (i in 1:Blocks) {
    ### Get the sample block
    if (i!=Blocks) {
      index<-((i-1)*gap+1):(gap*i)
    }else{## Last block contains gap+the remainder items
      index<-((i-1)*gap+1):n
    }
    sample<-x[index,]
    sd_sample<-sd(sample[,3])
    mean_sample<-mean(sample[,3])
    upper_bound<-mean_sample+1.5*sd_sample
    lower_bound<-mean_sample-1.5*sd_sample
    
    ## Get the in the range data
    index<-which(sample[,3]<=upper_bound&sample[,3]>=lower_bound)
    newmatrix<-rbind(newmatrix,sample[index,])
  }
  index<-which(newmatrix[,1]>=quantile(newmatrix[,1],0.15)&newmatrix[,1]<=quantile(newmatrix[,1],0.85))
  newmatrix<-newmatrix[index,]
  colnames(newmatrix)<-c("X","X^2","Y")
  return(newmatrix)
} 

### Outliers Removal Function: 15 by default in a group
OutlierRemovalY<-function(x,gap=15){## x is a n*2 matrix 
  x<-x[order(x[, 1]), ]
  n<-nrow(x)
  ### Decides how many blocks are needed for outlier removal
  Blocks<-n%/%gap
  if (Blocks==0){
    Blocks<-1
  }
  Remaining<-n%%gap
  
  ### Outlier Identifying
  newmatrix<-matrix(numeric(0),ncol=3)
  for (i in 1:Blocks){
    ### Get the sample block
    if (i!=Blocks){
      index<-((i-1)*gap+1):(gap*i)
    }else{## Last block contains gap+the remainder items
      index<-((i-1)*gap+1):n
    }
    
    sample<-x[index,]
    
    sd_sample<-sd(sample[,3])
    mean_sample<-mean(sample[,3])
    upper_bound<-mean_sample+1.5*sd_sample
    lower_bound<-mean_sample-1.5*sd_sample
    
    ## Get the in the range data
    index<-which(sample[,3]<=upper_bound&sample[,3]>=lower_bound)
    newmatrix<-rbind(newmatrix,sample[index,])
  }
  colnames(newmatrix)<-c("X","X^2","Y")
  return(newmatrix)
} 

clean_data <- function(x){
  index <- which(colnames(x)=='polygon/username')
  x=as.data.frame(x)
  x=x[,-(1:index)]
  return(x)
}


###### General Fit
library(readxl) 
# Set wd to Expert
setwd("C:\\Users\\liuyu\\OneDrive\\Documents\\UCLA\\Statistics 141SL\\Dr. Rootman - Project 1 Data (Eyelid Contour)-20190429\\UpperEyelidExpert\\Upper eyelid excel data points")
expert<-read_excel("5_1.xlsx")
# Set wd to Participants
setwd("C:\\Users\\liuyu\\OneDrive\\Documents\\UCLA\\Statistics 141SL\\Dr. Rootman - Project 1 Data (Eyelid Contour)-20190429\\UpperEyelidDataNonExpert\\DataPoints Clean")
a <- read_excel("5_1.xlsx") ### Can be changed into different rows
a <- clean_data(a)

### Wrap data to get column
par(mfrow=c(1,2))
#a2<-HalfEyeLid(a,c(0.006,0.00))
a3<-HalfEyeLid(a) ### Actual one will be used
a1_2<-WrapMatrixOld(a3) ### cleaned data
#a1_1<-WrapMatrixOld(a2)
a1_1old<-WrapMatrixOld(a) ### original data

### Clean Outlier
a1_1wrap<-OutlierRemovalXY(a1_2)
a1_1wrap1<-OutlierRemovalY(a1_2)

### Get a Matrix for the Expert data
expert<-expert[order(expert$X),]
expert<-cbind(expert,rep(NA,nrow(expert)))
expert[,3]<-expert[,2]
expert[,2]<-expert[,1]^2
colnames(expert)<-c("X","X^2","Y")
expert<-as.data.frame((expert))

### Fit into linear model
model1_1_part<-lm(Y~X+`X^2`,data=a1_1wrap)
model1_1_part1<-lm(Y~X+`X^2`,data=a1_1wrap1)
model1_1_expert<-lm(Y~X+`X^2`,data=expert)

#### Construct Mix Model

Outer<-which(expert$X<quantile(expert$X,0.3)|expert$X>quantile(expert$X,0.7))

exp_pred<-predict(model1_1_part,newdata = expert[-Outer,])
exp_pred_1<-predict(model1_1_part1,newdata = expert[Outer,])

Reconstruct<-data.frame("X"=expert$X[Outer],"X^2"=expert$`X^2`[Outer],"Y"=exp_pred_1)
Reconstruct<-rbind(Reconstruct,data.frame(
  "X"=expert$X[-Outer],"X^2"=expert$`X^2`[-Outer],"Y"=exp_pred))
colnames(Reconstruct)<-c("X","X^2","Y")

Reconstruct<-Reconstruct[order(Reconstruct$X),]


model_true<-lm(Y~X+`X^2`,data=Reconstruct)

exp_pe<-predict(model1_1_expert,newdata=expert)
exp_mix<-predict(model_true)
  


#### Model 1:Prediction
exp_orig_XY<-predict(model1_1_part,newdata = expert)
exp_orig_Y<-predict(model1_1_part1,newdata = expert)
mean((exp_pe-expert$Y)^2)## Benchmark MSE
mean((exp_mix-expert$Y)^2)
mean((exp_orig_Y-expert$Y)^2)## Look at this


par(mfrow=c(1,2))
plot(Y~X,data=a1_1old,main="Fitted vs. Actual")
lines(expert$Y~expert$X,col="red") # expert point connected
lines(exp_mix~X,data=expert,col="Blue") # mixed model data quad
lines(exp_pe~X,data=expert,col="green") # expert data quad

plot(Y~X,data=a1_1wrap)

plot(Y~X,data=a1_1old,main="Fitted vs. Actual")
lines(expert$Y~expert$X,col="red")
lines(exp_orig_Y~X,data=expert,col="orange") # method Y predicted quad
lines(exp_orig_XY~X,data=expert,col="purple") # method XY predicted quad
lines(exp_pe~X,data=expert,col="green")
#Y<-0.002598*expert$`X^2`-2.085177*expert$X+737.002304
#lines(Y~expert$X,col="cyan") # last group predicted quad


### Set bootstrap Times
#K=500 

### Set Upper Limit number of participants that is needed to test
#N=30

#MSE_mix<-matrix(rep(NA,(N-2)*K),ncol=K)


# ### Complicated
# for(j in 10:N){
#   MSE_J<-rep(NA,K)
#   for ( i in 1:K){
#   sample_index<-sample(1:nrow(a3),size = j ,replace=T)
#   boot_a<-a3[sample_index,]
#   boot_a<-WrapMatrixOld(boot_a)
#   boot_aY<-OutlierRemovalY(boot_a)
#   boot_aXY<-OutlierRemovalXY(boot_a)
#   model_XY<-lm(Y~X+`X^2`,data=boot_aXY)
#   model_Y<-lm(Y~X+`X^2`,data=boot_aY)
#   Outer<-which(expert$X<quantile(expert$X,0.3)|expert$X>quantile(expert$X,0.7))
#   Reconstruct.Outer<-data.frame("X"=expert$X[Outer],"X^2"=expert$`X^2`[Outer])
#   Reconstruct.Inner<-data.frame("X"=expert$X[-Outer],"X^2"=expert$`X^2`[-Outer])
#   colnames(Reconstruct.Inner)<-c("X","X^2")
#   colnames(Reconstruct.Outer)<-c("X","X^2")
#   Predict.Inner<-predict(model_XY,newdata=Reconstruct.Inner)
#   Predict.Outer<-predict(model_Y,newdata=Reconstruct.Outer)
#   Reconstruct.Outer<-cbind(Reconstruct.Outer,"Y"=Predict.Outer)
#   Reconstruct.Inner<-cbind(Reconstruct.Inner,"Y"=Predict.Inner)
#   Reconstruct<-rbind(Reconstruct.Inner,Reconstruct.Outer)
#   Reconstruct<-Reconstruct[order(Reconstruct$X),]
#   colnames(Reconstruct)<-c("X","X^2","Y")
#   expert<-expert[order(expert$X),]
#   model_mix<-lm(Y~X+`X^2`,data=Reconstruct)
#   MSE_J[i]<-mean((predict(model_mix,newdata=expert)-expert$Y)^2)
#   }
#   MSE_mix[j-9,]<-MSE_J
# }
# 
# apply(MSE_mix,1,mean)
# 
# apply(MSE_mix,1,sd)


### Use this Simple Loop instead of the previous one!!
### Set bootstrap Times
K=500 
### Set Upper Limit number of participants that is needed to test
N=30
MSE<-matrix(rep(NA,(N-2)*K),ncol=K)
for(j in 3:N){
  MSE_J<-rep(NA,K)
  for (i in 1:K) {
    sample_index<-sample(1:nrow(a3),size = j ,replace=T)
    boot_a<-a3[sample_index,]
    boot_a<-WrapMatrixOld(boot_a)
    boot_aY<-OutlierRemovalY(boot_a)
    model_Y<-lm(Y~X+`X^2`,data=boot_aY)
    Predict.Y<-predict(model_Y,newdata=expert)
    MSE_J[i]<-mean((Predict.Y-expert$Y)^2)
  }
  MSE[j-2,]<-MSE_J
}

apply(MSE,1,mean)[c(3, 13, 23)]
apply(MSE,1,sd)[c(3, 13, 23)]

## result plot and the range
apply(MSE,1,mean)
apply(MSE,1,sd)
N <- c(1:28)
data1 <- as.data.frame(cbind(N, mse_mean))
data2 <- as.data.frame(cbind(N, mse_sd))
par(mfrow=c(2,1))
ggplot(data1, aes(N,mse_mean))+geom_point()+geom_smooth(fill="white", color="#1ABBBB")+ylim(0, 80)+geom_hline(yintercept=42.25788, color = "gold", size=1.3)+theme_minimal()+geom_hline(yintercept=56.12896, linetype="dashed", color = "#3A5270", size=1.3)+theme(plot.title=element_text(hjust=0.5,size=30), axis.text=element_text(size=20), axis.title=element_text(size=24, face="bold"))+labs(title="MSE", x ="N: people needed", y = "MSE", colour="Cylinders")
ggplot(data2, aes(N,mse_sd))+geom_point()+geom_smooth(fill="white", color="#1ABBBB")+labs(title="Variance of MSE", x ="N: people needed", y = "Var(MSE)", colour="Cylinders")+theme_minimal()+theme(plot.title=element_text(hjust=0.5,size=30), axis.text=element_text(size=20), axis.title=element_text(size=24, face="bold"))

mse_mean <- apply(MSE,1,mean)
mse_sd <- apply(MSE,1,sd)
converge_mean <- rep(NA, 27)
for(i in 1:27){
  if (((mse_mean[i]-mse_mean[i+1])/(mse_mean[i])<=0.05)&((mse_mean[i]-mse_mean[i+1])/(mse_mean[i])>=-0.05))
    converge_mean[i] <- TRUE
  else
    converge_mean[i] <- FALSE
}
converge_sd <- rep(NA, 27)
for(i in 1:27){
  if (((mse_sd[i]-mse_sd[i+1])/(mse_sd[i])<=0.05)&((mse_sd[i]-mse_sd[i+1])/(mse_sd[i])>=-0.05))
    converge_sd[i] <- TRUE
  else
    converge_sd[i] <- FALSE
}
converge_mean
converge_sd



save(MSE,file = "MSE5_1.Rdata")
