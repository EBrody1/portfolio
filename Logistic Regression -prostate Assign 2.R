# Evan Brody  UMGC Data 630 Assignment #2
# Logistic Regression and NB Classifier for Prostate Cancer Data
#import the data
prostate <- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/630/module_2/prostate.csv",as.is = FALSE)

#remove the unique identifier column
prostate$ID<-NULL

# set up the catagorical variables as factor so R interprates them correctly add labels 
# so results are more understandable some of the residual plots require numbers not labels for 
# dependent variable so labels are only used in the visulizations not the model
str(prostate)
prostate$CAPSULE<-factor(prostate$CAPSULE)#, levels = 0:1, labels = c("No","Yes"))
prostate$RACE<-factor(prostate$RACE, levels = 1:2, labels = c("White","Black"))
prostate$DPROS<-factor(prostate$DPROS, levels = 1:4, labels = c("No Nodule","Unilobar Nodule (Left)","Unilobar Nodule  (Right)", "Bilobar Nodule"))
prostate$DCAPS<-factor(prostate$DCAPS, levels = 1:2, labels = c("No","Yes"))

# find the Number of Missing Values for each Variable
apply(prostate, 2, function (pros) sum(is.na(pros)))
#check if they are in different rows
prostate[!complete.cases(prostate),]
# check if vol is correlated with other numeric variables
cor(prostate$VOL, prostate$PSA, use = "complete.obs")
cor(prostate$VOL, prostate$AGE,  use = "complete.obs")
cor(prostate$VOL, prostate$GLEASON,  use = "complete.obs")
# Replacing missing value with the mean
prostate$VOL[is.na(prostate$VOL)]<-mean(prostate$VOL, na.rm=TRUE)
# check if Na is still present
summary(prostate$VOL)
# explore the race variable which has an NA
summary(prostate$RACE)
# there are significantly more of race = 1 fill in na with mode
prostate$RACE[is.na(prostate$RACE)]<- "White"
# check to make sure there are no more NA
# Display the Rows with Missing Values
prostate[!complete.cases(prostate),]

# explore variables with visuals and summary
summary(prostate)
# distributions 
barplot(summary(prostate$CAPSULE),col = c(4,2), xlab = 'Capsular Penetration', main= 'Distribution of Capsular Penetration', ylab = 'Number of Cases')
sapply(names(prostate), function(cname){
  #  only plot the numeric columns
  if (is.numeric(prostate[[cname]]))
    # put column name as plot title
    hist(prostate[[cname]], main=cname)
})

# visulaize how diff variables vary in relation to the dependent variable
boxplot(prostate$GLEASON ~ CAPSULE, data = prostate, main= 'Gleason Scores and Penetration', ylab = 'Gleason Score', xlab = 'Capsular Penetration')
boxplot(prostate$PSA ~ CAPSULE, data = prostate)
boxplot(prostate$AGE ~ CAPSULE, data = prostate)
boxplot(prostate$VOL ~ CAPSULE, data = prostate, main= 'Tumor volume by Capsular Penetration', ylab = 'Tumor Volume', xlab = 'Capsular Penetration')
barplot(table(prostate$CAPSULE, prostate$RACE),beside = T, legend.text = T)
barplot(table(prostate$CAPSULE, prostate$DCAPS),beside = T,  xlab = 'Capsular Detected', main = 'Capsular Detection During Rectal Exam', legend.text = T, 
        args.legend=list(title="Capsular Penetration"))
barplot(table(prostate$CAPSULE, prostate$DPROS),beside = T,legend.text = T, main= 'Nodulars Detected and Penetration')

# remove an outlier for certain models this was based on redidual plots
# this is commented out for the final model
#prostate<- prostate[-292, ]

# Set up the model 
#make sure that the result is reproducible
set.seed(1234)
#split the data into a training and test set
ind <- sample(2, nrow(prostate), replace = TRUE, prob = c(0.7, 0.3))
train.data <- prostate [ind == 1, ]
test.data <- prostate [ind == 2, ]


#build the model and store in a variable model
model<-glm(CAPSULE~., family=binomial, data=train.data)
summary(model)

# check the metrics of prediction with training data
library(caret)
# use the model to generate predictions
mypredictions<-round(predict (model, train.data, type="response"))
# reformat the predictions to use with the caret library
preds <- as.factor(mypredictions)
realized  <- as.factor(train.data$CAPSULE)
# generate confusion matrix and metrics
confusionMatrix(preds, realized, positive='1')
# diff set of metrics
confusionMatrix(preds, realized, mode="prec_recall", positive='1')
# visulize the confusion matrix
mosaicplot(table (mypredictions, train.data$CAPSULE), shade=TRUE, main="Predicted vs. Actual")

# now run the model on test data and take a look at a sampling 
predict (model, test.data, type="response")[1:10]
#store the estimated values in a variable mypredictions
mypredictions<-round(predict (model, test.data, type="response"))
# reformat to generate metrics with caret library
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$CAPSULE)
confusionMatrix(preds, realized, positive='1')
confusionMatrix(preds, realized, mode="prec_recall", positive='1')
mosaicplot(table (mypredictions, test.data$CAPSULE), shade=TRUE, main="Predicted Test set vs. Actual")

#plot an  roc curve
library(ROCR)                                           #Load ROCR to memory
# generate predictions
ROCRpred <- prediction(mypredictions, test.data$CAPSULE)
# evaluate performance
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
# plot
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line
# AUC metric 
performance(ROCRpred, measure = "auc")@y.values[[1]]

# plot a performance recall curve
prpred<-  performance(ROCRpred, 'prec','rec')
plot(prpred)

# plot a  sensitivity specifity curve
perf <- performance(ROCRpred, "sens", "spec")
plot(perf)

#minimal adequate model and metrics
model2 <- step(model)
summary(model2)

# predict with the model using test data
mypredictions<-round(predict (model2, test.data, type="response"))
# generate the metrics and confusion matrix
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$CAPSULE)
confusionMatrix(preds, realized, positive='1')
confusionMatrix(preds, realized, mode="prec_recall", positive='1')
mosaicplot(table (mypredictions, test.data$CAPSULE), shade=TRUE, main="Predicted Test set vs. Actual")                                           #Load ROCR to memory
# Roc and Auc 
ROCRpred <- prediction(mypredictions, test.data$CAPSULE)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line
performance(ROCRpred, measure = "auc")@y.values[[1]]

# compare the two models
anova(model, model2, test = "Chisq")

# tune the model by adjusting the threshold for classification
# get the model to predict the probabilites for each entry 
odds <- predict (model, test.data, type="response")
# set the predictions based on desired threshold
thresh <- .4
mypredictions <- ifelse(odds > thresh, 1, 0)
# check the metrics
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$CAPSULE)
confusionMatrix(preds, realized, positive='1')
confusionMatrix(preds, realized, mode="prec_recall", positive='1')
# check the ROc and AUC
ROCRpred <- prediction(mypredictions, test.data$CAPSULE)
performance(ROCRpred, measure = "auc")@y.values[[1]]
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line

# check the performance recall curve for improvements 
prpred<-  performance(ROCRpred, 'prec','rec')
plot(prpred)

#plot the residuals
plot(predict(model),residuals(model), col=c("blue"))
lines(lowess(predict(model),residuals(model)), col=c("black"), lwd=2)
abline(h=0, col="grey")
# 4 plots of residuals
plot(model)

#10.visualize the Effect displays of each variable
#install.packages("effects")
library(effects)
plot(allEffects(model))


# Naive Bayes
library("arules")
library("e1071")
# reformat as catagorical variables for the algorithm and check results
prostate$GLEASON<-discretize(prostate$GLEASON, method="frequency", breaks=6)
summary(prostate$GLEASON)
prostate$VOL<-discretize(prostate$VOL, method="frequency", breaks=6)
summary(prostate$VOL)
prostate$PSA<-discretize(prostate$PSA, method="frequency", breaks=6)
summary(prostate$PSA)
hist(prostate$AGE)
prostate$AGE<- cut(prostate$AGE, 4)
summary(prostate$AGE)   # After

#split the transformed data into a training and test set
ind <- sample(2, nrow(prostate), replace = TRUE, prob = c(0.7, 0.3))
train.data <- prostate [ind == 1, ]
test.data <- prostate [ind == 2, ]

# initiate the NB model 
model<-naiveBayes(CAPSULE~., train.data)
#output the model
model
# prediction with the test data
mypredictions<-predict(model, test.data, type="class")
# reformat in order to generate metrics
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$CAPSULE)
confusionMatrix(preds, realized, positive='1')
confusionMatrix(preds, realized, mode="prec_recall", positive='1')

# tune the threshold for classification
# generate prediciton values not classifications
odds <- predict (model, test.data, type="raw")
thresh <- .45
mypredictions <- ifelse(odds[,2] > thresh, 1, 0)
# metrics
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$CAPSULE)
confusionMatrix(preds, realized, positive='1')
confusionMatrix(preds, realized, mode="prec_recall", positive='1')
# ROC and AUC 
ROCRpred <- prediction(mypredictions, test.data$CAPSULE)
performance(ROCRpred, measure = "auc")@y.values[[1]]
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line

