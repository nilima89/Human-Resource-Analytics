Prm(list=ls())
library(rpart) #for decision tree
library(rattle)
library(RColorBrewer)
library(rpart.plot) #for decision tree
library(ggplot2) #for various viz
library(corrplot) #for correlation plot
library(caTools) #for sampling the data
library(randomForest) #for random forest
library(e1071) #for svm
library(quadprog) #for svm
library(Boruta)
library(ROCR)

set.seed(1234)

#reading data from csv
hrdata=read.csv(file.choose())
hrdata=data.frame(hrdata)

#running data metrics
str(hrdata)
summary(hrdata)

#correlation plot for the data set
corrplot(cor(hrdata[,1:8]), method="circle")

#CLASSIFICATION

#converting sales and salary to numeric factors
sales <- unique(hrdata$sales)
hrdata$sales <- as.numeric(1:10)[match(hrdata$sales, sales)] 
hrdata$salary <- as.numeric(1:3)[match(hrdata$salary, c('low', 'medium', 'high'))]

#splitting the dataset into training and testing data
pd=sample(2,nrow(hrdata),replace=TRUE,prob=c(0.6,0.4))
train.hrdata=hrdata[pd==1,]
test.hrdata=hrdata[pd==2,]

#plotting the data points 
xi1=train.hrdata$satisfaction_level
xi2=train.hrdata$last_evaluation
xi3=train.hrdata$number_project
xi4=train.hrdata$average_montly_hours
xi5=train.hrdata$time_spend_company
xi6=train.hrdata$Work_accident
xi8=train.hrdata$promotion_last_5years
xi9=train.hrdata$sales
xi10=train.hrdata$salary
xi=cbind(xi1,xi2,xi3,xi4,xi5,xi6,xi8,xi9,xi10)
yi=train.hrdata$left
xy=data.frame(cbind(xi,yi))
#data plot
plot(xi,pch=(yi+2),col=(yi+3),cex=1) 

#building the Decision Tree 
dtm1=rpart(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years,train.hrdata, method="class")
print(dtm1)
rpart.plot(dtm1)
fancyRpartPlot(dtm1)
dtm1$cptable[which.min(dtm1$cptable[,"xerror"]),"CP"] #gives the CP corresponding to the minimum cross validation error
pdtm1=prune(dtm1,cp=0.01) #pruning the decision tree with the cp value for from the above statement
rpart.plot(pdtm1) #plotting the pruned versioned of the tree
fancyRpartPlot(pdtm1)
printcp(pdtm1)
#calculating classification error
pred_table_hrdata = table(predict(pdtm1,test.hrdata, type="class"), test.hrdata$left)
classification_error = 1-sum(diag(pred_table_hrdata))/sum(pred_table_hrdata)
classification_error
#classification error is 0.031 for test data, therefore accuracy is around 97 percent

#implementing Random Forests with 1000 trees
rf.model=randomForest(as.factor(left) ~ satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years, data=train.hrdata, nsize=20, ntree=1000)
predict.rf=predict(rf.model, test.hrdata)
table(test.hrdata$left, predict.rf) #confusion matrix
mean(predict.rf==test.hrdata$left) #accuracy

#constructing svm model
svm.model=svm(left ~ satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years, data=train.hrdata,gamma=0.25, cost=10)
summary(svm.model)
predicted_svm=predict(svm.model, test.hrdata)
predicted_svm=ifelse(predicted_svm > 0.5,1,0)
table(test.hrdata$left, predicted_svm) #confusion matrix
mean(predicted_svm==test.hrdata$left) #accuracy=0.9588

# Plotting ROC for all models 
# Decision tree
predict_dt_ROC=predict(pdtm1, test.hrdata)
pred_dt=prediction(predict_dt_ROC[,2], test.hrdata$left)
perf_dt=performance(pred_dt, "tpr", "fpr")

# Random forest
predict_rf_ROC=predict(rf.model, test.hrdata, type="prob")
pred_rf=prediction(predict_rf_ROC[,2], test.hrdata$left)
perf_rf=performance(pred_rf, "tpr", "fpr")

# SVM
predict_svm_ROC=predict(svm.model, test.hrdata, type="response")
pred_svm=prediction(predict_svm_ROC, test.hrdata$left)
perf_svm=performance(pred_svm, "tpr", "fpr")

# Plotting the three curves - ROC 
plot(perf_dt, main = "ROC curves for the models", col='blue')
plot(perf_rf, add=TRUE, col='green3')
plot(perf_svm, add=TRUE, col='darkmagenta')
legend('bottom', c("Decision Tree", "Random Forest", "Support Vector Machine"), fill = c('blue','green3','darkmagenta'), bty='n')


