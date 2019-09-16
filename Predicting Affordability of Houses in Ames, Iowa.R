### 2.1 Data Cleaning

## cleaning training dataset
training_total <- read.csv("C:/Users/liuyu/OneDrive/Documents/UCLA/Statistics 101C/Final Project/HTrainLast.csv")[, -1]
# delete observations with Y is NA
NonNA2 <- training_total[is.na(training_total[[80]])==FALSE, ]
# deal with correlation terms
basement_sub <- which(is.na(NonNA2[[30]])|is.na(NonNA2[[31]])|is.na(NonNA2[[32]])|is.na(NonNA2[[33]])|is.na(NonNA2[[34]])|
                        is.na(NonNA2[[36]])|is.na(NonNA2[[37]])|is.na(NonNA2[[38]]))
NonNA2[basement_sub, c(30:33)] <- NA
NonNA2[basement_sub, c(34, 36:38)] <- 0
garage_sub <- which(is.na(NonNA2[[58]])|is.na(NonNA2[[59]])|is.na(NonNA2[[60]])|is.na(NonNA2[[61]])|is.na(NonNA2[[62]])|
                      is.na(NonNA2[[63]])|is.na(NonNA2[[64]]))
NonNA2[garage_sub, c(58, 60, 63:64)] <- NA
NonNA2[garage_sub, c(61:62)] <- 0
# deal with numerical variables: NA to 0
nume2 <- c(26, 47, 48)
for(i in 1:length(nume2)){
  NonNA2[[nume2[i]]] <- replace(NonNA2[[nume2[i]]], is.na(NonNA2[[nume2[i]]])==TRUE, 0)
}
# deal with numerical variables: NA by predicting via MLR 
library(leaps)
numeric <- c(3, 4, 17:20, 26, 34, 36:38, 43:52, 54, 56, 59, 61, 62, 66:71, 75:77)
NonNA2_num <- NonNA2[, numeric]
train_3 <- NonNA2_num[is.na(NonNA2[[3]])==FALSE, -24]
test_3 <- NonNA2_num[is.na(NonNA2[[3]]), -c(1, 24)]
regfit.full3=regsubsets(LotFrontage~., train_3)
summary(regfit.full3)
reg.summary3 <- summary(regfit.full3)
which.min(reg.summary3$bic)
m3 <- lm(LotFrontage~LotArea+OverallQual+X1stFlrSF+FullBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+GarageArea+PoolArea, data=train_3)
summary(m3)
predict3 <- round(predict(m3, test_3))
for(i in 1:560)
  NonNA2[is.na(NonNA2[[3]]), 3] <- predict3[i]
train_59 <- NonNA2_num[is.na(NonNA2[[59]])==FALSE, -1]
test_59 <- NonNA2_num[is.na(NonNA2[[59]]), -c(1, 24)]
regfit.full59=regsubsets(GarageYrBlt~., train_59)
summary(regfit.full59)
reg.summary59 <- summary(regfit.full59)
which.min(reg.summary59$bic)
m59 <- lm(GarageYrBlt~YearBuilt+YearRemodAdd+MasVnrArea+X1stFlrSF+LowQualFinSF+FullBath+BedroomAbvGr+Fireplaces+GarageArea, data=train_59)
summary(m59)
predict59 <- round(predict(m59, test_59))
for(i in 1:162)
  NonNA2[is.na(NonNA2[[59]]), 59] <- predict59[i]
# deal with categorical variables: NA to a category None
cate <- c(6, 25, 30:33, 35, 57:58, 60, 63:64, 72:74)
for(i in 1:length(cate)){
  NonNA2[[cate[i]]] <- as.character(NonNA2[[cate[i]]])
  NonNA2[[cate[i]]][is.na(NonNA2[[cate[i]]])] <- "None"
  NonNA2[[cate[i]]] <- as.factor(NonNA2[[cate[i]]])
}
# deal with categorical variables: NA to a others
NonNA2[[2]] <- as.character(NonNA2[[2]])
NonNA2[[2]][is.na(NonNA2[[2]])] <- "RL"
NonNA2[[2]] <- as.factor(NonNA2[[2]])
NonNA2[[9]] <- as.character(NonNA2[[9]])
NonNA2[[9]][is.na(NonNA2[[9]])] <- "AllPub"
NonNA2[[9]] <- as.factor(NonNA2[[9]])
NonNA2[[42]] <- as.character(NonNA2[[42]])
NonNA2[[42]][is.na(NonNA2[[42]])] <- "SBrkr"
NonNA2[[42]] <- as.factor(NonNA2[[42]])
NonNA2[[53]] <- as.character(NonNA2[[53]])
NonNA2[[53]][is.na(NonNA2[[53]])] <- "TA"
NonNA2[[53]] <- as.factor(NonNA2[[53]])
NonNA2[[55]] <- as.character(NonNA2[[55]])
NonNA2[[55]][is.na(NonNA2[[55]])] <- "Typ"
NonNA2[[55]] <- as.factor(NonNA2[[55]])
# delete the unused variable column 9
NonNA3 <- NonNA2[, -9]
# transform the first column into factor
NonNA3[, 1] <- as.factor(NonNA3[, 1])
# transform the Year remodeled column to categorical
for(i in 1:3498){
  if(NonNA3[i, 19]==NonNA3[i, 18])
    NonNA3[i, 19] <- "No"
  else
    NonNA3[i, 19] <- "Yes"
}
NonNA3[, 19] <- as.factor(NonNA3[, 19])
# transform the Year built column to how many years it had been built
sort(NonNA3$YearBuilt, decreasing = TRUE)[1]
NonNA3$YearBuilt <- 2010-NonNA3$YearBuilt

## cleaning testing dataset
test_total <- read.csv("C:/Users/liuyu/OneDrive/Documents/UCLA/Statistics 101C/Final Project/HTestLastNoY.csv")[-1]
NonNA2 <- test_total
basement_sub <- which(is.na(NonNA2[[30]])|is.na(NonNA2[[31]])|is.na(NonNA2[[32]])|is.na(NonNA2[[33]])|is.na(NonNA2[[34]])|
                        is.na(NonNA2[[36]])|is.na(NonNA2[[37]])|is.na(NonNA2[[38]]))
NonNA2[basement_sub, c(30:33)] <- NA
NonNA2[basement_sub, c(34, 36:38)] <- 0
garage_sub <- which(is.na(NonNA2[[58]])|is.na(NonNA2[[59]])|is.na(NonNA2[[60]])|is.na(NonNA2[[61]])|is.na(NonNA2[[62]])|
                      is.na(NonNA2[[63]])|is.na(NonNA2[[64]]))
NonNA2[garage_sub, c(58, 60, 63:64)] <- NA
NonNA2[garage_sub, c(61:62)] <- 0
nume2 <- c(26, 47, 48)
for(i in 1:length(nume2)){
  NonNA2[[nume2[i]]] <- replace(NonNA2[[nume2[i]]], is.na(NonNA2[[nume2[i]]])==TRUE, 0)
}
library(leaps)
numeric <- c(3, 4, 17:20, 26, 34, 36:38, 43:52, 54, 56, 59, 61, 62, 66:71, 75:77)
NonNA2_num <- NonNA2[, numeric]
train_3 <- NonNA2_num[is.na(NonNA2[[3]])==FALSE, -24]
test_3 <- NonNA2_num[is.na(NonNA2[[3]]), -c(1, 24)]
regfit.full3=regsubsets(LotFrontage~., train_3)
summary(regfit.full3)
reg.summary3 <- summary(regfit.full3)
which.min(reg.summary3$bic)
m3 <- lm(LotFrontage~LotArea+BsmtFinSF1+X2ndFlrSF+TotRmsAbvGrd+GarageArea+EnclosedPorch+PoolArea, data=train_3)
summary(m3)
predict3 <- round(predict(m3, test_3))
for(i in 1:225)
  NonNA2[is.na(NonNA2[[3]]), 3] <- predict3[i]
train_59 <- NonNA2_num[is.na(NonNA2[[59]])==FALSE, -1]
test_59 <- NonNA2_num[is.na(NonNA2[[59]]), -c(1, 24)]
regfit.full59=regsubsets(GarageYrBlt~., train_59)
summary(regfit.full59)
reg.summary59 <- summary(regfit.full59)
which.min(reg.summary59$bic)
m59 <- lm(GarageYrBlt~OverallCond+YearBuilt+YearRemodAdd+X1stFlrSF+LowQualFinSF+Fireplaces+GarageArea+EnclosedPorch+PoolArea, data=train_59)
summary(m59)
predict59 <- round(predict(m59, test_59))
for(i in 1:91)
  NonNA2[is.na(NonNA2[[59]]), 59] <- predict59[i]
cate <- c(6, 25, 30:33, 35, 57:58, 60, 63:64, 72:74)
for(i in 1:length(cate)){
  NonNA2[[cate[i]]] <- as.character(NonNA2[[cate[i]]])
  NonNA2[[cate[i]]][is.na(NonNA2[[cate[i]]])] <- "None"
  NonNA2[[cate[i]]] <- as.factor(NonNA2[[cate[i]]])
}
NonNA2[[2]] <- as.character(NonNA2[[2]])
NonNA2[[2]][is.na(NonNA2[[2]])] <- "RL"
NonNA2[[2]] <- as.factor(NonNA2[[2]])
NonNA2[[9]] <- as.character(NonNA2[[9]])
NonNA2[[9]][is.na(NonNA2[[9]])] <- "AllPub"
NonNA2[[9]] <- as.factor(NonNA2[[9]])
NonNA2[[23]] <- as.character(NonNA2[[23]])
NonNA2[[23]][is.na(NonNA2[[23]])] <- "VinylSd"
NonNA2[[23]] <- as.factor(NonNA2[[23]])
NonNA2[[24]] <- as.character(NonNA2[[24]])
NonNA2[[24]][is.na(NonNA2[[24]])] <- "Vinylsd"
NonNA2[[24]] <- as.factor(NonNA2[[24]])
NonNA2[[42]] <- as.character(NonNA2[[42]])
NonNA2[[42]][is.na(NonNA2[[42]])] <- "SBrkr"
NonNA2[[42]] <- as.factor(NonNA2[[42]])
NonNA2[[53]] <- as.character(NonNA2[[53]])
NonNA2[[53]][is.na(NonNA2[[53]])] <- "TA"
NonNA2[[53]] <- as.factor(NonNA2[[53]])
NonNA2[[55]] <- as.character(NonNA2[[55]])
NonNA2[[55]][is.na(NonNA2[[55]])] <- "Typ"
NonNA2[[55]] <- as.factor(NonNA2[[55]])
NonNA2[[78]] <- as.character(NonNA2[[78]])
NonNA2[[78]][is.na(NonNA2[[78]])] <- "WD"
NonNA2[[78]] <- as.factor(NonNA2[[78]])
NonNA3 <- NonNA2[, -9]
NonNA3[, 1] <- as.factor(NonNA3[, 1])
for(i in 1:1500){
  if(NonNA3[i, 19]==NonNA3[i, 18])
    NonNA3[i, 19] <- "No"
  else
    NonNA3[i, 19] <- "Yes"
}
NonNA3[, 19] <- as.factor(NonNA3[, 19])
sort(NonNA3$YearBuilt, decreasing = TRUE)[1]
NonNA3$YearBuilt <- 2010-NonNA3$YearBuilt


### 2.2 Model Exploration

## read the training data
data <- read.csv("TrainNonNA.csv")[,-1]

## read the test data
testdata <- read.csv("TestNonNA.csv")[, -1]
testdata <- rbind(data[1, -79], testdata)
testdata <- testdata[-1, ]
rownames(testdata) <- 1:1500
totalData <- rbind(data[,-79], testdata)
for (f in 1:length(names(totalData))){
  levels(data[, f]) <- levels(totalData[, f])
  levels(testdata[, f]) <- levels(totalData[, f])
}

## Logistic on full data
library(car)
library(caret)
model_full <- glm(data = data, affordabilitty~.,family = binomial)
alias(model_full)
# REMOVE ALIAS
data <- as.data.frame(data[,-c(37,45)])
# Divide data into numerical and categorical
numeric <- rep(NA,ncol(data))
for (i in 1:ncol(data)) {
  if(is.numeric(data[,i])==TRUE)
    numeric[i] <- i
}
numeric <- numeric[!is.na(numeric)]
data_numeric <- data[,numeric]
data_factor <- data [, -numeric]
for (i in 1:42) {
  data_factor[,i] <- as.factor(data_factor[,i])
}

## LDA (0.920877)
train <- data_numeric[sampling,]
test <- data_numeric[-sampling,]
testx <- test[,-18]
lda_model_numeric <- lda(train$affordabilitty ~.,data = train)
lda_model_numeric
predict_lda1 <- predict(lda_model_numeric,testx)
table(predict_lda1$class,test$affordabilitty)

## QDA(0.914204)
qda_model_numeric <- qda(train$affordabilitty ~.,data = train)
qda_model_numeric
predict_qda1 <- predict(qda_model_numeric,testx)
table(predict_qda1$class,test$affordabilitty)

## KNN (0.9466158,k=1/ 0.950 second/ 0.95615, k=1,cut=0.7,scale)
train <- data_numeric[sampling,]
test <- data_numeric[-sampling,]
testx <- scale(test[,-33])
trainx <- scale(train[,-33])
trainy <- train[,33]
knn_model_numeric<-NA
error <- NA
for (i in 1:10) {
  knn_model_numeric <- knn(trainx,testx,trainy,k=i )
  error[i]<- mean(test$affordabilitty!= knn_model_numeric)
}
print(min(error))
print(which(error == min(error)))
knn_model_numeric <- knn(trainx,testx,trainy,k=1)

## NUMERIC
# Remove high correlation
df <- cor(data_numeric[,-35])#Remove y-variable
high <- findCorrelation(df,cutoff = 0.8)
high <- sort(high)
data_numeric <- data_numeric[,-high]#Variables with high correlation
log_numerical <- glm(affordabilitty~., data = data_numeric,family = binomial)
colnames(data_numeric)[high]
vif(log_numerical) #combining the result of VIF and FINDCORRELATION, we finalize the highly correlated variables.
library(car)
# Continue variable selection using stepAIC
stepAIC <- stepAIC(log_numerical)
data_numeric_new <- data_numeric[,-c(27,5,7,25,32,24,18,20,15,3,1,13,30)]#20NUMERIC(INCLUDING RESPONSE) LEFT
# TRAIN VS TEST with finalized numeric predictors
set.seed(123)
sampling <- sample(nrow(data_numeric), 2449, replace=FALSE)
train <- data_numeric_new[sampling,]
test <- data_numeric_new[-sampling,]
# KNN using finalized numerical predictors(0.9466158,k=1/ 0.950 second/ 0.95615, k=1,cut=0.7,scale )
train <- data_numeric_new[sampling,]
test <- data_numeric_new[-sampling,]
testx <- scale(test[,-20])
trainx <- scale(train[,-20])
trainy <- train[,20]
knn_model_numeric<-NA
error <- NA
for (i in 1:10) {
  knn_model_numeric <- knn(trainx,testx,trainy,k=i )
  error[i]<- mean(test$affordabilitty!= knn_model_numeric)
}
print(min(error))
print(which(error == min(error)))
knn_model_numeric <- knn(trainx,testx,trainy,k=1)
predict <- predict(log_model,testx)
predict <- ifelse(predict>0.5,1,0)
table(predict,test$affordabilitty)

## FACTOR
# Identify high correlation using CHI-SQUARE
result<- matrix(rep(NA,42*42),nrow = 42,ncol = 42)
for (i in 1:ncol(data_factor)) {
  for (j in 1:ncol(data_factor)) {
    result[i,j] <-  (chisq.test(data_factor[,i],data_factor[,j])$p.value)>0.5
  }
}
indep <- NA
for (i in 1:ncol(result)) {
  indep[i] <- sum(result[i,])
}

## FACTOR LOGISTIC(0.9189)
model_factor_log <- glm(affordabilitty~.,data = data_factor, family = binomial)
summary(model_factor_log)
set.seed(123)
sampling <- sample(nrow(data_factor), 2449, replace=FALSE)
train <- data_factor[sampling,]
test <- data_factor[-sampling,]
testx <- test[,-43]
log_model_factor <- glm(train$affordabilitty ~.,data = train, family = binomial())
summary(log_model_factor)
predict <- predict(log_model_factor,testx)
predict <- ifelse(predict>0.5,1,0)
table(predict,test$affordabilitty)
mean(predict==test$affordabilitty)

## stepaic
stepAIC_factor <- stepAIC(log_model_factor,direction = "forward")
summary(stepAIC_factor)
predict <- predict(stepAIC_factor,testx)
predict <- ifelse(predict>0.5,1,0)
table(predict,test$affordabilitty)

## NEW FACTOR LOGISTIC(0.9294) 
model_newfactor_log <- glm(affordabilitty~.,data = data_factor_new, family = binomial)
train <- data_factor_new[sampling,]
test <- data_factor_new[-sampling,]
testx <- test[,-36]
predict <- predict(model_newfactor_log,testx)
predict <- ifelse(predict>0.5,1,0)
table(predict,test$affordabilitty)

## COMBINED logisticc, factor and numeric(0.9542421, correlation cutoff=0.4/0.9513823, cutoff=0.8)
data_new <- cbind(data_factor,data_numeric_new)
train<- data_new[sampling,]
test<- data_new[-sampling,]
model_glm_combine <- glm(affordabilitty~.,data = train,family = binomial)
predict <- predict(model_glm_combine,test[,1:55])
predict <- ifelse(predict>0.5,1,0)
table(predict,test$affordabilitty)
mean(predict==test$affordabilitty)


### 2.3 Finalizing Model Selection

## seprate train and test
sampling <- sample(nrow(data), 2449, replace=FALSE)
train <- data[sampling,]
test <- data[-sampling,]
testx <- test[, -79]

## tree
tree.train=tree(affordabilitty~.,train)
tree.pre <- predict(tree.train,testx,type="class")
#Use cross validation to choose the best 
tree.cv <- cv.tree(tree.train,FUN=prune.misclass)
qplot(tree.cv$size, tree.cv$dev, geom=c("point", "line"), xlab="size", ylab= "deviance")
prune <- prune.misclass(tree.train,best=6)
prune.pre <- predict(prune,testx,type="class")
table(prune.pre, test$affordabilitty)

## bagging
library(randomForest)
bagging_train <- randomForest(affordabilitty~., data=train, mtry=78, importance=TRUE)
rank <- sort(importance(bagging_train)[, 3], decreasing = TRUE)
varImpPlot(bagging_train)
bagging_pred <- predict(bagging_train, newdata=testx, type='class')
bagging_test_error <- mean(bagging_pred!=test$affordabilitty)
bagging_test_error

## random forest
tuneRF(data[, c(16, 11, 45, 37, 42, 18, 48, 43, 33, 61)], data[,79], stepFactor=1.5)
bagging_total <- randomForest(affordabilitty~OverallQual+Neighborhood+GrLivArea+TotalBsmtSF+X1stFlrSF+YearBuilt+FullBath+X2ndFlrSF+
                                BsmtFinSF1+GarageArea, data=data, mtry=4, ntree=300, importance=TRUE)
total.pred <- predict(bagging_total, newdata=testdata, type='class')
# write.csv(total.pred, file="kaggle1.csv")
