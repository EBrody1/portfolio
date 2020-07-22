# Evan Brody
# UMGC DATA 630 Assignment # 4
#import the data
whas1 <- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/630/ann/whas1.csv")
# drop id variable
whas1$ID <- NULL
# find which variables need transformation
summary(whas1)
# Number of Missing Values for Each Variable
apply(whas1, 2, function (whas1) sum(is.na(whas1)))
# explore 
# labels for graphs don't run for model
#whas1$FSTAT<-factor(whas1$FSTAT, levels = 0:1, labels = c("Survived","Expired"))
barplot(summary(as.factor(whas1$DSTAT)), main= 'Survived Original Hospitalization', legend.text = 'Yes = 0 , No = 1')
barplot(summary(as.factor(whas1$FSTAT)),main = 'Survived')

# find skewed variables for the whole data set
sapply(names(whas1), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(whas1[[cname]]))
    # use the `main` param to put column name as plot title
    hist(whas1[[cname]], main=cname, xlab = cname)
})

# plot continous variables by the target variable
boxplot(whas1$AGE ~ FSTAT, data = whas1, main = 'Age By Survival', ylab= 'Age', xlab = 'Final Status')
boxplot(whas1$LENFOL ~ FSTAT, data = whas1, main = 'Length of Time Until Final Followup By Survival', ylab= 'Time', xlab = 'Final Status')
boxplot(whas1$LENSTAY ~ FSTAT, data = whas1, main = 'Length of Hospital Stay By Survival', ylab= 'Time', xlab = 'Final Status')

# correlation with target variable 
sapply(names(whas1), function(cname){
  # (make sure we only include the numeric columns)
  if (is.numeric(whas1[[cname]]))
    cor(whas1$FSTAT,whas1[[cname]]) 
})
# transform variables scale numeric continous 
whas1[1]<-scale(whas1[1])
whas1$CPK <- scale(whas1$CPK)
whas1$LENFOL <- scale(whas1$LENFOL)
whas1$LENSTAY <- scale(whas1$LENSTAY)
# set the categorical variables as factor
whas1$YEAR <- as.factor(whas1$YEAR)
whas1$YRGRP <- as.factor(whas1$YRGRP)
whas1$MITYPE<- as.factor(whas1$MITYPE)
# convert to dummy variables
matrix.data <- model.matrix(~ ., data=whas1)
# check out names of new variables
head(matrix.data)

#make sure that the result is reproducible
set.seed(12345)
#split the data into a training and test set
ind <- sample(2, nrow(matrix.data), replace = TRUE, prob = c(0.7, 0.3))
train.data <- matrix.data[ind == 1, ]
test.data <- matrix.data[ind == 2, ]
head(train.data)

library(neuralnet)
# set the model variables
f<- FSTAT ~ AGE + SEX + CPK + SHO + CHF + MIORD + MITYPE2 + MITYPE3 + YEAR2 + YEAR3 + YEAR4 + YEAR5 + YEAR6 + YRGRP2 + YRGRP3 + LENSTAY + DSTAT + LENFOL
# initiate the model
nn<-neuralnet(formula =f, data = train.data, hidden=2, err.fct="ce", linear.output = FALSE)

# model metrics
# errors
Train_Error <- nn$result.matrix[1,1]
paste("CE Error: ", round(Train_Error, 3)) 

AIC <- nn$result.matrix[4,1]
paste("AIC: ", round(AIC,3))

BIC <- nn$result.matrix[5,1]
paste("BIC: ", round(BIC, 3))

#Model evaluation for training set 
mypredict<-compute(nn, nn$covariate)$net.result
# then round the predicted probabilities
mypredict<-apply(mypredict, c(1), round)

# confusion matrix for the training set
table(mypredict, train.data[,20], dnn =c("Predicted", "Actual"))
mean(mypredict==train.data[,20])

# confusion matrix for the test set and metrics
# compute the predictions on the test set
testPred <- compute(nn, test.data[, 0:19])$net.result
testPred<-apply(testPred, c(1), round)

# compare predictions to actual test labels
library(caret)
preds <- as.factor(testPred)
realized  <- as.factor(test.data[,20])
confusionMatrix(preds, realized)
result <- confusionMatrix(preds, realized, mode="prec_recall")
result

# ROC AUc
library(ROCR)
detach(package:neuralnet)
nn.pred <- prediction(testPred, test.data[,20])
pref <- performance(nn.pred, "tpr", "fpr")
# plot the curve
plot(pref)
# add reference line
abline(a=0, b=1)
#Calculate the AUC value
perf_AUC=performance(nn.pred,"auc") 
AUC=perf_AUC@y.values[[1]]
AUC
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

library(NeuralNetTools)
# variable importance
olden(nn)
# set the column names for the graphs
cols <- c('AGE', 'SEX', 'CPK', 'SHO', 'CHF','MIORD', 'MITYPE2', 'MITYPE3', 'YEAR2', 'YEAR3',  'YEAR4',  'YEAR5', 'YEAR6', 'YRGRP2', 'YRGRP3', 'LENSTAY', 'DSTAT', 'LENFOL')
library(neuralnet)

# plot effects of a variable does it vary or around zero
sapply(cols, function(cname){
  gwplot(nn,selected.covariate = cname, min = -2.5, max = 5)
})
# plot model structure
plot(nn)


# tune for optimal number of hidden layers
formula = f
Model <- train(form=formula,     
               data=train.data,           
               method="neuralnet",   
               ### Parameters for layers
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)),               
               ### Parameters for optmization
               learningrate = 0.01,  
               threshold = 0.01,     
               stepmax = 50000         
)
# show how the models compare
plot(Model)
# best hyperparameters 
Model$bestTune

# use them in a model then repeat steps from above
nn2<-neuralnet(formula =f, data = train.data, hidden=c(1), err.fct="ce", linear.output = FALSE)


# errors
Train_Error <- nn2$result.matrix[1,1]
paste("CE Error: ", round(Train_Error, 3)) 

AIC <- nn2$result.matrix[4,1]
paste("AIC: ", round(AIC,3))

BIC <- nn2$result.matrix[5,1]
paste("BIC: ", round(BIC, 3))

# confusion matrix for the test set
testPred <- compute(nn2, test.data[, 0:19])$net.result
testPred<-apply(testPred, c(1), round)
preds <- as.factor(testPred)

realized  <- as.factor(test.data[,20])
confusionMatrix(preds, realized, positive = '1')
result <- confusionMatrix(preds, realized, mode="prec_recall", positive = '1')
result
# plot the confusion matrix
mosaicplot(table (testPred, test.data[,20]), shade=TRUE, main="Predicted Test set vs. Actual")                                           
detach(package:neuralnet)
nn.pred = prediction(testPred, test.data[,20])
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)
abline(a=0, b=1)
perf_AUC=performance(nn.pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
# model structure
plotnet(nn2)
plot(nn2)
nn2
# inital weights
nn2$startweights
# final model weights
nn2$result.matrix
# varaible importance
olden(nn2)
library(neuralnet)
# plot effects of a variable see if it varies or is  around zero
sapply(cols, function(cname){
  gwplot(nn,selected.covariate = cname, min = -2.5, max = 5)
})

# tune for hidden layers with a different learning rate
formula = f
Model <- train(form=formula,     
               data=train.data,           
               method="neuralnet",   
               ### Parameters for layers
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)),               
               ### Parameters for optmization
               learningrate = 0.0001,  
               threshold = 0.01,     
               stepmax = 50000         
)
plot(Model)
Model$bestTune

nn3<-neuralnet(formula = f, data = train.data, hidden=c(1,2), err.fct="ce", linear.output = FALSE, learningrate = .0001)

# errors
Train_Error <- nn3$result.matrix[1,1]
paste("CE Error: ", round(Train_Error, 3)) 
# confusion matrix for the test set
testPred <- compute(nn3, test.data[, 0:19])$net.result
testPred<-apply(testPred, c(1), round)
preds <- as.factor(testPred)
realized  <- as.factor( test.data[,20])
confusionMatrix(preds, realized)
result <- confusionMatrix(preds, realized, mode="prec_recall")
result
detach(package:neuralnet)
nn.pred = prediction(testPred, test.data[,20])
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)
abline(a=0, b=1)
perf_AUC=performance(nn.pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
plot(nn3)
nn3$result.matrix
library(neuralnet)
# plot effects of a variable does it vary or around zero
sapply(cols, function(cname){
  gwplot(nn,selected.covariate = cname, min = -2.5, max = 5)
})
plotnet(nn3)
olden(nn3)

# try different ann algorithm on the data
library("nnet")
library ("NeuralNetTools")
nn <- nnet(FSTAT~., data=train.data, size=4)
summary(nn)  #List the network weights
nn$n        #number of nodes in each layer

# plot and visualization
plotnet(nn)

#Relative importance for each variable; the network may have >=1 hidden layers >=1 output
olden(nn)

#Classification accuracy for training data
mypredict<-predict(nn)
mypredict<-apply(mypredict, c(1), round)            # Round the predicted probabilities
table(mypredict, train.data[,20], dnn =c("Predicted", "Actual"))
mean(mypredict==train.data[,20])

# Evaluate the model on a test data
testPred <- predict(nn, test.data[,1:19])
testPred<-apply(testPred, c(1), round)
table(testPred, test.data[,20], dnn =c("Predicted", "Actual"))
mean(testPred==test.data[,20])

# Confusion matrix implementation in caret package
confusionMatrix(table(testPred, test.data[,20]), dnn=c("predicted", "actual"))


detach(package:neuralnet)
nn.pred = prediction(testPred, test.data[,20])
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)
abline(a=0, b=1)
perf_AUC=performance(nn.pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
olden(nn)


