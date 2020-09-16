#Data Loading
setwd("C:/Users/Karthik/Documents/Lakshmi - Back up docs/Learning/BABI/Sessions/S10/ML Assign")
Data<-read.csv("Cars.csv")
nrow(Data)
View(Data)
str(Data)
summary(Data)
Data1<-Data

#outlier function
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

## Build a function to calculate percentage of missing values in Columns and Rows
pMiss = function(x){
  sum(is.na(x))/length(x)*100
}

#EDA
par(mfrow=c(1, 1))
boxplot(Data$Age)
summary(boxplot(Data$Age))
boxplot.stats(Data$Age)$out 
outlierKD(Data, Age)

Er_gender<-aggregate(Data$Engineer,by=list(Category=Data$Gender), FUN=sum)
Er_gender

MBA_gender<-aggregate(Data$MBA,by=list(Category=Data$Gender), FUN=sum)
MBA_gender

par(mfrow=c(1, 1))
boxplot(Data$Work.Exp)
summary(boxplot(Data$Work.Exp))
aggregate(Data$Work.Exp,by=list(Category=Data$Gender), FUN=mean)
boxplot.stats(Data$Work.Exp)$out 
outlierKD(Data, Work.Exp)

par(mfrow=c(1, 1))
boxplot(Data$Salary)
summary(boxplot(Data$Salary))
aggregate(Data$Salary,by=list(Category=Data$Gender), FUN=mean)
boxplot.stats(Data$Salary)$out 
outlierKD(Data, Salary)

par(mfrow=c(1, 1))
boxplot(Data$Distance)
summary(boxplot(Data$Distance))
boxplot.stats(Data$Distance)$out 
outlierKD(Data, Distance)

par(mfrow=c(1, 1))
hist(Data$Age)

hist(Data$Engineer)

hist(Data$MBA)

hist(Data$Work.Exp)

hist(Data$Salary)

hist(Data$Distance)

hist(Data$license)

plot(Data$Transport,Data$Age)

plot(Data$Transport,Data$Gender)

plot(Data$Gender,Data$Transport)

plot(Data$Transport,Data$Work.Exp)

plot(Data$Transport,Data$license)

plot(Data$license,Data$Transport)

# Normality check for distance

shapiro.test(Data$Distance)

library(ggplot2)
library(ggpubr)
library(caret)
library(car)
library(DMwR)
library('ggplot2') # visualization
library('scales') # visualization
library('AER') #Coefficients
require("tidyr")
library('corrplot')
library('purrr')
library('coefplot')
library('psych')
library('MASS')
library('leaflet.extras')
library("PerformanceAnalytics")
library('GPArotation')
library('MVN')
library('psych')
library('MASS')
library('psy')
library('corpcor')
library('nnet')
library('plyr')
library("e1071")
library('ggcorrplot')
library('mlogit')  # for multiple class logistic regression
library('caTools')
# library('InformationValue')
library('rpart.plot')
library('ggplot2')
library('RColorBrewer')
library('dummies') # for converting categorical into dummy one
library('caret')
library('pscl') ## for  McFadden R2
library('StatMeasures')
library('sqldf')
library('purrr')
library('tidyr')
library('caret')
library('ggplot2')
library('gains')
library('lubridate')
library('dummies')
library('glmnet')
library('gbm')
#install.packages("VIM",dependencies = TRUE)
library('VIM')  ### This is for knn
library('haven')

hist(Data$Work.Exp, col = 'blue')
hist(Data$license, col = 'green')
vis_summary <- ggplot(Data, aes(x = Data$Salary, y = Data$Work.Exp)) +
  facet_grid(~ Data$Gender + Data$Transport)+
  geom_boxplot(na.rm = TRUE, colour = "#3366FF",outlier.colour = "red", outlier.shape = 1) +
  labs(x = "Work Experience", y = "Salary") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme(legend.position="bottom", legend.direction="horizontal")
vis_summary
vis_summary$notchupper

# Correlation plot
my_num_data <- Data[, sapply(Data, is.numeric)]
cor.Data<-cor(my_num_data)
round(cor.Data,2)
library(corrplot)
corrplot(cor.Data,method="circle",bg="white")


## The columns Engineer,MBA and license need to be converted into factors
Data$Engineer<-as.factor(Data$Engineer)
Data$MBA<-as.factor(Data$MBA)
Data$license<-as.factor(Data$license)

## Find Percentage of missing values in each column
col_miss = apply(Data,2,pMiss)  ## 2 is for Columns
col_miss

#KNN imputation
carsbasedata<-knnImputation(Data)
summary(carsbasedata)

#Revised dataset after missing value imputation
carsbasedata_final <- subset(carsbasedata, select = Age:Transport)
carsbasedata_final_boost <- subset(carsbasedata, select = Age:Transport)
carsbasedata_final_logit <- subset(carsbasedata, select = Age:Transport)
nrow(carsbasedata_final)

#Understanding of data balancing nature
table(carsbasedata_final$Transport)
print(prop.table(table(carsbasedata_final$Transport)))

carsbasedata$CarUsage<-ifelse(carsbasedata$Transport =='Car',1,0)
table(carsbasedata$CarUsage)
sum(carsbasedata$CarUsage == 1)/nrow(carsbasedata)
carsbasedata$CarUsage<-as.factor(carsbasedata$CarUsage)
summary(carsbasedata_final)
View(carsbasedata)


##Split the data into test and train
set.seed(400)
carindex<-createDataPartition(carsbasedata$CarUsage, p=0.7,list = FALSE,times = 1)
carsdatatrain<-carsbasedata[carindex,]
carsdatatest<-carsbasedata[-carindex,]
prop.table(table(carsdatatrain$CarUsage))
prop.table(table(carsdatatest$CarUsage))
carsdatatrain<-carsdatatrain[,c(1:8,10)]
carsdatatest<-carsdatatest[,c(1:8,10)]
## The train and test data have almost same percentage of cars usage as the base data
## Apply SMOTE on Training data set
library(DMwR)
attach(carsdatatrain)
carsdataSMOTE<-SMOTE(CarUsage~., carsdatatrain, perc.over = 250,perc.under = 150)
prop.table(table(carsdataSMOTE$CarUsage))
##Create control parameter for GLM
outcomevar<-'CarUsage'
regressors<-c("Age","Work.Exp","Salary","Distance","license","Engineer","MBA","Gender")
trainctrl<-trainControl(method = 'repeatedcv',number = 10,repeats = 3)
carsglm<-train(carsdataSMOTE[,regressors],carsdataSMOTE[,outcomevar],method = "glm", family = "binomial",trControl = trainctrl)
summary(carsglm$finalModel)
carglmcoeff<-exp(coef(carsglm$finalModel))
write.csv(carglmcoeff,file = "Coeffs.csv")
varImp(object = carsglm)
plot(varImp(object = carsglm), main="Vairable Importance for Logistic Regression")
carusageprediction<-predict.train(object = carsglm,carsdatatest[,regressors],type = "raw")
confusionMatrix(carusageprediction,carsdatatest[,outcomevar], positive='1')
carusagepreddata<-carsdatatest
carusagepreddata$predictusage<-carusageprediction
carunknown<-read.csv("Cars2.csv", header = TRUE)
carunknown$license<-as.factor(carunknown$license)
carunknown$Engineer<-as.factor(carunknown$Engineer)
carunknown$MBA<-as.factor(carunknown$MBA)
carunknown$predictcaruse<-predict.train(object = carsglm,carunknown[,regressors],type = "raw")
print(carunknown)

trainctrlgn<-trainControl(method = 'cv',number = 10,returnResamp = 'none')
carsglmnet<-train(CarUsage~Age+Work.Exp+Salary+Distance+license, data = carsdataSMOTE, method = 'glmnet', trControl = trainctrlgn)
carsglmnet
varImp(object = carsglmnet)
plot(varImp(object = carsglmnet), main="Vairable Importance for Logistic Regression - Post Ridge Regularization")
carusagepredictiong<-predict.train(object = carsglmnet,carsdatatest[,regressors],type = "raw")
confusionMatrix(carusagepredictiong,carsdatatest[,outcomevar], positive='1')
## Let us predict for unknown cases
carunknown$predictcarusegn<-predict.train(object = carsglmnet,carunknown[,regressors],type = "raw")
print(carunknown)
#Inference & Prediction Using Linear Discriminant Analysis
##Split the original base data into test and train samples again
carsbasedatalda<-read.csv("cars.csv", header = TRUE)
carsbasedatalda$Gender<-as.factor(carsbasedatalda$Gender)
carsbasedatalda$Engineer<-as.factor(carsbasedatalda$Engineer)
carsbasedatalda$MBA<-as.factor(carsbasedatalda$MBA)
carsbasedatalda<-knnImputation(carsbasedatalda)
set.seed(400)
carindexlda<-createDataPartition(carsbasedatalda$Transport, p=0.7,list = FALSE,times = 1)
carstrainlda<-carsbasedatalda[carindexlda,]
carstestlda<-carsbasedatalda[-carindexlda,]
carstrainlda$license<-as.factor(carstrainlda$license)
carstestlda$license<-as.factor(carstestlda$license)
cartrainlda.car<-carstrainlda[carstrainlda$Transport %in% c("Car", "Public Transport"),]
cartrainlda.twlr<-carstrainlda[carstrainlda$Transport %in% c("2Wheeler", "Public Transport"),]
cartrainlda.car$Transport<-as.character(cartrainlda.car$Transport)
cartrainlda.car$Transport<-as.factor(cartrainlda.car$Transport)
cartrainlda.twlr$Transport<-as.character(cartrainlda.twlr$Transport)
cartrainlda.twlr$Transport<-as.factor(cartrainlda.twlr$Transport)
prop.table(table(cartrainlda.car$Transport))
prop.table(table(cartrainlda.twlr$Transport))
carldatwlrsm <- SMOTE(Transport~., data = cartrainlda.twlr, perc.over = 150, perc.under=200)
table(carldatwlrsm$Transport)
carldacarsm <- SMOTE(Transport~., data = cartrainlda.car, perc.over = 175, perc.under=200)
table(carldacarsm$Transport)
carldacar<-carldacarsm[carldacarsm$Transport %in% c("Car"),]
carsdatatrainldasm<-rbind(carldatwlrsm,carldacar)
str(carsdatatrainldasm)
table(carsdatatrainldasm$Transport)
## Build the model
attach(carsdatatrainldasm)
trainctrllda<-trainControl(method = 'cv',number = 10)
carslda<-train(Transport~Age+Work.Exp+Salary+Distance+license+Gender+Engineer+MBA ,data = carsdatatrainldasm, method="lda", trControl=trainctrllda)
carslda$finalModel
plot(varImp(object = carslda),main="Variable Importance for Linear Discriminant Analysis" )
carsldapredict<-predict.train(object = carslda,newdata = carstestlda)
confusionMatrix(carsldapredict,carstestlda[,9])
predictTransportuk<-predict.train(object = carslda,newdata = carunknown)
carunknown$predictTransport<-predictTransportuk
print(carunknown)
#Improve LDA Model by Regularization
trainctrlpda<-trainControl(method = 'cv',number = 10, returnResamp = 'all')
carspda<-train(Transport~Age+Work.Exp+Salary+Distance+license+Gender+Engineer+MBA ,data = carsdatatrainldasm, method="pda", trControl=trainctrlpda)
carspda$finalModel
carspda
plot(varImp(object = carspda), main="Variable Importance for Penalized Discriminant Analysis")
carspdapredict<-predict.train(object = carspda,newdata = carstestlda)
confusionMatrix(carspdapredict,carstestlda[,9])
predictTransportuk1<-predict.train(object = carslda,newdata = carunknown)
carunknown$predictTransportpda<-predictTransportuk1
print(carunknown)
#Prediction using CART
carscart<-train(Transport~.,carsdatatrainldasm,method = 'rpart', trControl = trainControl(method = 'cv',number = 5,savePredictions = 'final'))
carscart$finalModel
library(rattle)
fancyRpartPlot(carscart$finalModel)
predictions_CART<-predict(carscart,carstestlda)
confusionMatrix(predictions_CART,carstestlda$Transport)
predictTransportuk2<-predict.train(object = carscart,newdata = carunknown)
carunknown$predictTransportcart<-predictTransportuk2
print(carunknown)
#Prediction using Boosting
boostcontrol <- trainControl(number=10)

xgbGrid <- expand.grid(
  eta = 0.3,
  max_depth = 1,
  nrounds = 50,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1, subsample = 1
)

carsxgb <-  train(Transport ~ .,carsdatatrainldasm,trControl = boostcontrol,tuneGrid = xgbGrid,metric = "Accuracy",method = "xgbTree")

carsxgb$finalModel

predictions_xgb<-predict(carsxgb,carstestlda)
confusionMatrix(predictions_xgb,carstestlda$Transport)

predictTransportuk3<-predict.train(object = carsxgb,newdata = carunknown)
carunknown$predictTransportxgb<-predictTransportuk3
print(carunknown)

#Prediction Using Multinomial Logistic Regression
carsmlr<-train(Transport ~.,carsdatatrainldasm,method = "multinom")
carsmlr$finalModel
carmlrcoeff<-exp(coef(carsmlr$finalModel))
write.csv(carmlrcoeff,file = "Coeffsmlr.csv")
plot(varImp(object=carsmlr), main = "Variable Importance for Multinomial Logit")
predictions_mlr<-predict(carsmlr,carstestlda)
confusionMatrix(predictions_mlr,carstestlda$Transport)
predictTransportuk4<-predict.train(object = carsmlr,newdata = carunknown)
carunknown$predictTransportmlr<-predictTransportuk4
print(carunknown)
#Prediction using Random Forest
rftrcontrol<-control <- trainControl(method="repeatedcv", number=10, repeats=3)
mtry<-sqrt(ncol(carsdatatrainldasm))
tunegridrf <- expand.grid(.mtry=mtry)
carsrf<-train(Transport ~.,carsdatatrainldasm,method = "rf", trControl=rftrcontrol, tuneGrid = tunegridrf)
carsrf$finalModel
plot(varImp(object=carsrf), main = "Variable Importance for Random Forest")
predictions_rf<-predict(carsrf,carstestlda)
confusionMatrix(predictions_rf,carstestlda$Transport)
predictTransportuk5<-predict.train(object = carsrf,newdata = carunknown)
carunknown$predictTransportrf<-predictTransportuk5
print(carunknown)

# Bagging:

library(ipred)
library(rpart)
library(MASS)
gbag <- bagging(Transport ~ ., data = carsdatatrainldasm, coob=TRUE)
print(gbag)


scomb <- list(list(model=slda, predict=function(object, newdata)
  + predict(object, newdata)$x))

gbagc <- bagging(Transport ~ ., data = carsdatatrainldasm, comb=scomb)

predict(gbagc, newdata=carunknown)

