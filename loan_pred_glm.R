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
#install.packages('Amelia')
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


#create some imp features
#create features for train:
##coapplicant:applicant income
library(sqldf)
coapp_app_income  <- sqldf("select  Loan_ID, CoapplicantIncome/ApplicantIncome as coapp_ratio
                           from train_F")
train_F <- merge(train_F, coapp_app_income, by.x = "Loan_ID" , by.y = "Loan_ID"  )
train_F$coapp_ratio <- ifelse(is.na(train_F$coapp_ratio), 0, train_F$coapp_ratio)

per_head_cost<- sqldf("select  Loan_ID, ApplicantIncome/(Dependents+1) as per_head_cost
                      from train_F")

train_F <- merge(train_F, per_head_cost, by.x = "Loan_ID" , by.y = "Loan_ID"  )



#create features for test:
##coapplicant:applicant income
library(sqldf)
coapp_app_income  <- sqldf("select  Loan_ID, CoapplicantIncome/ApplicantIncome as coapp_ratio 
                           from test_F")
test_F <- merge(test_F, coapp_app_income, by.x = "Loan_ID" , by.y = "Loan_ID"  )
test_F$coapp_ratio <- ifelse(is.na(test_F$coapp_ratio), 0, test_F$coapp_ratio)


per_head_cost<- sqldf("select  Loan_ID, ApplicantIncome/(Dependents+1) as per_head_cost
                      from test_F")

test_F <- merge(test_F, per_head_cost, by.x = "Loan_ID" , by.y = "Loan_ID"  )

quantile(test_F1$ApplicantIncome , c(0.80, 0.85, 0.90, 0.95, 0.99, 1), na.rm = TRUE)


#treating outliers and normalising the data
plot(train_F$ApplicantIncome)
plot(train_F$CoapplicantIncome)
plot(train_F$ApplicantIncome)
FUN_OUTLIERS = function(X) (X - min(X))/(max(X) - min(X))

train_F$CoapplicantIncome<- FUN_OUTLIERS(train_F$CoapplicantIncome)
train_F$ApplicantIncome<- FUN_OUTLIERS(train_F$ApplicantIncome)
train_F$LoanAmount<-  FUN_OUTLIERS(train_F$LoanAmount)
train_F$coapp_ratio <-  FUN_OUTLIERS(train_F$coapp_ratio)
train_F$per_head_cost <-  FUN_OUTLIERS(train_F$per_head_cost)

FUN_CAP <- function(x){
  quantiles <- quantile( x, c(.01, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
train_F$CoapplicantIncome<- FUN_CAP(train_F$CoapplicantIncome)
train_F$ApplicantIncome<- FUN_CAP(train_F$ApplicantIncome)
train_F$LoanAmount<-  FUN_CAP(train_F$LoanAmount)


plot(train_F$ApplicantIncome)
plot(train_F$CoapplicantIncome)
plot(train_F$ApplicantIncome)
-----------
plot(test_F$ApplicantIncome)
plot(test_F$CoapplicantIncome)
plot(test_F$ApplicantIncome)

test_F$CoapplicantIncome<- FUN_OUTLIERS(test_F$CoapplicantIncome)
test_F$ApplicantIncome<- FUN_OUTLIERS(test_F$ApplicantIncome)
test_F$LoanAmount<-  FUN_OUTLIERS(test_F$LoanAmount)
test_F$coapp_ratio <-  FUN_OUTLIERS(test_F$coapp_ratio)
test_F$per_head_cost <-  FUN_OUTLIERS(test_F$per_head_cost)

test_F$CoapplicantIncome<- FUN_CAP(test_F$CoapplicantIncome)
test_F$ApplicantIncome<- FUN_CAP(test_F$ApplicantIncome)
test_F$LoanAmount<-  FUN_CAP(test_F$LoanAmount)


#creating dumies for the cat variables
library(dummies)
train_F_dumies <- dummy.data.frame(train_F, names = c("Gender","Married",
                                                      "Dependents","Education",
                                                      "Self_Employed","Property_Area"))

test_F_dumies <- dummy.data.frame(test_F, names = c("Gender","Married",
                                                      "Dependents","Education",
                                                      "Self_Employed","Property_Area"))


#Apply modelling algos
#in the train data create partitions to check the acc results
install.packages('caTools')
library(caTools)

#splitting train data in test and train and removing the id.
set.seed(88)
train_T<- train_F[1:nrow(train_F)*0.75,2:ncol(train_F)]
str(train_T)
train_V<- train_F[-(1:nrow(train_F)*0.75),2:ncol(train_F)]
str(train_V)

#apply FSA
library(data.table)
library(dplyr)
library(mlr)
library(Amelia)
library(MASS)
library(sqldf)
library(RSQLite)
library(randomForest)
library(glmnet)
library(xgboost)
library(nnet)
library(caret)
library(FactoMineR)
fnl_dataset <- FSA(data = train_F, 
                   dependent.variable = "Loan_Status", variable.not.required = "Loan_ID", 
                   depend.variable.type = "discrete")


fsa_subset_data <- data.frame(fnl_dataset[[1]])
fsa_feature_list_summary <- data.frame(fnl_dataset[[2]])
fsa_feature_list_full <- data.frame(fnl_dataset[[3]])

##-----------------------------------------------------------------------------------------------
## Apply GLM
##-----------------------------------------------------------------------------------------------
#apply glm using all variables.
model <- glm ( Loan_Status ~.
               , data = train_T, family = binomial)

model <- glm ( Loan_Status ~ ApplicantIncome+per_head_cost+LoanAmount+coapp_ratio+CoapplicantIncome
               +Credit_History
               , data = train_T , family = binomial)



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
#predict_test<-predict(model ,test_F, type = '

##-----------------------------------------------------------------------------------------------
## Apply GLM on test
##-----------------------------------------------------------------------------------------------
preds = predict(model, test_F, n.trees = 100)
test_F$Loan_Status <- preds
test_F$Loan_Status_bin <- ifelse(test_F$Loan_Status >= 0.5 , 'Y', 'N')
sub_logr <- data.frame(Loan_ID = test_F$Loan_ID, Loan_Status =  test_F$Loan_Status_bin)
write.csv(sub_logr, file = "sub_logr.csv", row.names = F)