#Loan Prediction

setwd("C:/Users/tpurohit/Desktop/AV/Loan Pred")
trainL<-read.csv("TrainAV_loanpred.csv",header=T)
str(trainL)
train1<- train 
testL<-read.csv("TestAV_loanpred.csv", header=T)
str(testL)


##Identifying Nulls in Data and treating it
testL$Loan_Status <- 'N'
combi <- rbind(trainL, testL)
str(combi)
install.packages('Amelia')
library(Amelia)
combi[combi == ""] <- NA
missmap(combi, main = "Missing values vs observed")
sum(is.na(combi)) #37347 values
sort(sapply(combi,function(x){sum(is.na(x))/length(x)}),decreasing =T) #percetage

#since the missing values are very less in percent imputing it using mean/median
#impute credit heistory with median
combi$Credit_History[is.na(combi$Credit_History)] <- median(combi$Credit_History, na.rm = TRUE)
#impute Loan amount  with mean
combi$LoanAmount[is.na(combi$LoanAmount)] <- 
  mean((combi$LoanAmount/combi$ApplicantIncome)*combi$ApplicantIncome, na.rm = TRUE)
#impute Loan_Amount_Term with median
combi$Loan_Amount_Term[is.na(combi$Loan_Amount_Term)] <- median(combi$Loan_Amount_Term, na.rm = TRUE)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#impute gender with median
combi$Gender[is.na(combi$Gender)] <- getmode(combi$Gender)
combi$Dependents[is.na(combi$Dependents)] <- getmode(combi$Dependents)
combi$Married[is.na(combi$Married)] <- getmode(combi$Married)
combi$Self_Employed[is.na(combi$Self_Employed)] <- getmode(combi$Self_Employed)

#sep test and train
train_F <- combi[1:nrow(trainL),]
str(train_F)
test_F <- combi[-(1:nrow(trainL)),]
str(test_F)


#Apply modelling algos
#in the train data create partitions to check the acc results
install.packages('caTools')
library(caTools)

#splitting train data in test and train and removing the id.
set.seed(88)
train_T<- train_F[1:nrow(trainL)*0.75,2:ncol(trainL)]
str(train_T)
train_V<- train_F[-(1:nrow(trainL)*0.75),2:ncol(trainL)]
str(train_V)

##-----------------------------------------------------------------------------------------------
## Apply GLM
##-----------------------------------------------------------------------------------------------
#apply glm using all variables.
model <- glm (Loan_Status ~ ., data = train_T, family = binomial)
summary(model)
predict <- predict(model ,train_V, type = 'response')
predict <- ifelse(predict > 0.5,'Y','N')
train_V$predict <- predict
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response')

?glm

#apply glm for sig variables.
model <- glm (Loan_Status ~ Education+LoanAmount+Loan_Amount_Term+Property_Area
              , data = train_T, family = binomial)
summary(model)
predict <- predict(model ,train_V, type = 'response')
predict <- ifelse(predict > 0.5,'Y','N')
train_V$predict <- predict
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)



##-----------------------------------------------------------------------------------------------
## Apply SVM
##-----------------------------------------------------------------------------------------------
install.packages("e1071")
library(e1071)



# create model
?svm
model_svm <- svm(Loan_Status ~ .,data=train_T,kernel='sigmoid',gamma=200,cost=200)
preds <- predict(model_svm,train_V)
table(train_V$Loan_Status, preds)
table(preds)
train_V$predict <- preds
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)


##-----------------------------------------------------------------------------------------------
## Apply RF
##-----------------------------------------------------------------------------------------------

install.packages("randomForest")
library(randomForest)
library(rpart)
library(glmnet)

?glmnet
?randomForest

model_rf <- randomForest(Loan_Status ~ .,data=train_T, mtry = 4,method= "class")
summary(model_rf)
preds <- predict(model_rf,train_V)
train_V$predict<- preds
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response')


##-----------------------------------------------------------------------------------------------
## Apply Neiv Bias
##-----------------------------------------------------------------------------------------------

library(e1071)

#Fitting model
model_nb <-naiveBayes(Loan_Status ~ .,data=train_T)
summary(model_nb)
preds <- predict(model_nb,train_V)
train_V$predict<- preds
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response')


##-----------------------------------------------------------------------------------------------
## Apply gbm
##-----------------------------------------------------------------------------------------------
library(robustbase)
install.packages("caret")
library(caret)
install.packages("gbm")
library(gbm)

?gbm


model_gbm =gbm(Loan_Status ~ .,data=train_T, distribution = "gaussian",n.trees = 10000,
               shrinkage = 0.01, interaction.depth = 4)
summary(model_gbm)
preds <- predict(model_gbm,train_V, n.trees = 1000)
train_V$predict<- preds
predict <- ifelse(preds > 1.5,'Y','N')
train_V$predict <- predict
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response')


##-----------------------------------------------------------------------------------------------
## Apply xgboost
##-----------------------------------------------------------------------------------------------

install.packages('xgboost', dependencies = T)
install.packages("data.table")
library(data.table)
library(xgboost)

?data
data(train_T, package='xgboost')
data(agaricus.test, package='xgboost')
train_T_N<-data.matrix(train_T)
str(train_T_N)
?xgboost
colnames(train_T_N)
train_T_N[,12] <-ifelse(train_T_N[,12] == 1,0,1)
model_xgb <- xgboost(data = train_T_N, label = train_T_N[,12],
                     nrounds = 2, objective = "reg:logistic")
train_V_N<-data.matrix(train_V)
train_V_N[,12] <-ifelse(train_V_N[,12] == 1,0,1)
preds = predict(model_xgb, train_V_N)
predict <- ifelse(preds > 0.5,'Y','N')
train_V$predict <- predict
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response'

?

##-----------------------------------------------------------------------------------------------
## Apply NN
##-----------------------------------------------------------------------------------------------
install.packages("neuralnet", dependencies = T)
library(neuralnet)
?neuralnet

install.packages("nnet")
library(nnet)
?multinom

model_NN<- multinom(Loan_Status~., data=train_T, maxit=500, trace=T)

library(caret)
mostImportantVariables <- varImp(model_NN)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))

preds = predict(model_NN, train_V)
train_V$predict <- preds
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response'
##----------------------------------------------------------------------------
#msc trials
##----------------------------------------------------------------------------  
str(train_T)
maxs <- apply(train_T, 2, max) 
mins <- apply(train_T, 2, min)
scaled <- as.data.frame(scale(train_T, center = mins, scale = maxs - mins))
neuralnet(Loan_Status~ ApplicantIncome , data=train_T, hidden=2, err.fct="logistic",linear.output=FALSE)
?na.strings
ifelse(train_T$Gender == "",1,0)
levels(train_T$Gender)
str(train_T)
dummies <- dummyVars(~ Gender +  Married + Dependents+Education + Self_Employed + Property_Area+Loan_Status
               , data = train_T)
?install.packages
install.packages('robustbase', repos =  
                   "https://cran.rstudio.com/bin/windows/contrib/3.3/robustbase_0.92-8.zip")
library(ROCR)
pr <- prediction(train_V$Loan_Status, train_V$predict)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
table(train_V, train_V$misClasificError)
?predict
#confusion matrix
pr <- prediction(predict, train_V$Loan_Status)
table(train_V$Loan_Status, predict > 0.5)
view(train_V)
fitted.results <- ifelse(predict > 0.5,'Y','N')
##----------------------------------------------------------------------------
##predicting on test data
##----------------------------------------------------------------------------
preds = predict(model_rf, test_F)
test_F$Loan_Status <- preds
test_F$Loan_Status_bin <- ifelse(test_F$Loan_Status >= 0.5 , 'Y', 'N')
sub_logr <- data.frame(Loan_ID = test_F$Loan_ID, Loan_Status =  test_F$Loan_Status_bin)
write.csv(sub_logr, file = "sub_logr.csv", row.names = F)

###rf
sub_rf <- data.frame(Loan_ID = test_F$Loan_ID, Loan_Status =  test_F$Loan_Status)
write.csv(sub_rf, file = "sub_rf.csv", row.names = F)

##xg boost
test_f_xgb<-data.matrix(test_F)
preds = predict(model_xgb, test_f_xgb)
test_F$Loan_Status <- preds
test_F$Loan_Status_bin <- ifelse(test_F$Loan_Status >= 0.5 , 'Y', 'N')
sub_xgb <- data.frame(Loan_ID = test_F$Loan_ID, Loan_Status =  test_F$Loan_Status_bin)
write.csv(sub_xgb, file = "sub_xgb.csv", row.names = F)

