voice <- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/630/module_3/voice.csv", header=TRUE)

voice$label <- factor(voice$label)

# explore
# find number of missing values
nrow(voice[!complete.cases(voice),])

# show distribution for ach variable in the data set
sapply(names(voice), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(voice[[cname]]))
    # use the `xlab` param to put column name 
    hist(voice[[cname]], main= 'Distribution', xlab= cname)
})

barplot(summary(voice$label))
# check out differences for males and females to see if there are 2 overlapping distributions
hist(voice$IQR[voice$label == 'male'])
hist(voice$IQR[voice$label == 'female'])
# summary stats
summary(voice)


# check if variable are correlated to meanfun, meanfreq, meandom and spectoral flatness
voice1 <- voice[,1:20] # exclude non numeric variable for correlation
cor(voice1, use = "complete.obs")[,13]
cor(voice1, use = "complete.obs")[,1]
cor(voice1, use = "complete.obs")[,16]
cor(voice1, use = "complete.obs")[,10]
# heatmap of matrix
cormat <- round(cor(voice1),1)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
ggheatmap <-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# add the coefficients
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
  )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#plot of target variable related to other variables
sapply(names(voice), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(voice[[cname]]))
    # use the `main` param to put column name as plot title
    boxplot(voice[[cname]]~ label, data = voice, main=cname, xlab= 'Gender', ylab= cname)
})

#split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(voice), replace = TRUE, prob = c(0.7, 0.3))
train.data <- voice[ind == 1, ]
test.data <- voice[ind == 2, ]

library(rpart)
fit<-  rpart(label~., method="class", data = train.data)


printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


# prune the tree with cp number 
pfit<-prune(fit, cp=   fit$cptable[2,"CP"])
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Voice")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# predict with the model
mypredictions<-predict(pfit, test.data, type="class")


# confusion matrix and metrics
library(caret)
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$label)
confusionMatrix(preds, realized)
confusionMatrix(preds, realized, mode="prec_recall")

library(ROCR)
pred <- predict(pfit, newdata = test.data, type = "prob") 
t1.scores <- prediction(pred[,2], test.data$label)
t1.perf <- performance(t1.scores, "tpr", "fpr")

# Plot the ROC curve
plot(t1.perf, col = "green", lwd = 1.5, main = 'ROC Curve for RPART')
#AUC
t1.auc <- performance(t1.scores, "auc")
AUC=t1.auc@y.values[[1]]
AUC
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

# tune the model
library(e1071)
audit.rpart <- tune.rpart(myFormula, data = train.data, cp = c(0.002,0.005,0.01,0.015,0.02,0.03),
                          maxdepth = 1:7, minsplit=seq(10,100,10))
summary(audit.rpart)

# use the tuned model
model2<-rpart(label~., train.data, control=rpart.control( cp= .002, maxdepth = 5, minsplit= 20))
plot(model2, uniform=TRUE)
text(model2, use.n=TRUE, all=TRUE, cex=.8)

mypredictions<-predict(model2, test.data, type="class")

# confusion matrix and metrics
preds <- as.factor(mypredictions)
realized  <- as.factor(test.data$label)
confusionMatrix(preds, realized)
confusionMatrix(preds, realized, mode="prec_recall")

# ROc AUC
pred <- predict(model2, newdata = test.data, type = "prob") 
t1.scores <- prediction(pred[,2], test.data$label)
t1.perf <- performance(t1.scores, "tpr", "fpr")

# Plot the ROC curve
plot(t1.perf, col = "green", lwd = 1.5, main = 'ROC Curve')
#AUC
t1.auc <- performance(t1.scores, "auc")
t1.auc@y.values[[1]]
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

# check out variable importance
model2$variable.importance 
barplot(modle2$variable.importance, main = 'Variable Importance')


# use ctree algorithm
library("party")

# Run the method on a training data
myFormula<-label~.

model<- ctree(myFormula, data = train.data)

#output the tree structure
print(model) 

#visualize the tree
nodes(model, 2)
plot(model)
#visualize the tree differently
plot(model, type="simple")

plot(model, type="simple",           # no terminal plots
     inner_panel=node_inner(model,
                            abbreviate = TRUE,            # short variable names
                            pval = FALSE,                 # no p-values
                            id = T),                  # no id of node
     terminal_panel=node_terminal(model, 
                                  abbreviate = TRUE,
                                  digits = 1,             # few digits on numbers
                                  fill = c("white"),      # make box white not gray
                                  id = FALSE)
)

#confusion matrix
table(predict(model), train.data$label)

#Evaluate the model on test data
testPred <- predict(model, newdata = test.data)
table (testPred, test.data$label)

# metrics
preds <- as.factor( predict(model, newdata = test.data))
realized  <- as.factor(test.data$label)
confusionMatrix(preds, realized)
confusionMatrix(preds, realized, mode="prec_recall")

library(pROC)
#Draw the ROC curve 
ctree.ROC <- roc(predictor=as.numeric(testPred),
                 response=test.data$label)
#Area under the curve 
ctree.ROC$auc
plot(ctree.ROC,main="ctree ROC")

# tune the model to reduce complexity
model2 <- ctree(myFormula, data = train.data, control = ctree_control(maxdepth = 2))
print(model2)

#visualize the tree
plot(model2, type="simple")
plot(model2)

#Evaluate the model on a test data
testPred <- predict(model2, newdata = test.data)
table (testPred, test.data$label)

# metrics
preds <- as.factor( predict(model2, newdata = test.data))
realized  <- as.factor(test.data$label)
confusionMatrix(preds, realized)
confusionMatrix(preds, realized, mode="prec_recall")

#Draw the ROC curve 
ctree.ROC <- roc(predictor=as.numeric(testPred),
                 response=test.data$label)
#Area under the curve 
ctree.ROC$auc
plot(ctree.ROC,main="ctree ROC")


# try random forest 
library(randomForest)
fit <- randomForest(label~., data = train.data)
# mtr is number of features used per tree, maxnodes is maximum nodes
rf <- randomForest(class ~ ., data= train.data, ntree=100, mtry=2, importance=TRUE, proximity=TRUE)
print(fit) # view results
importance(fit) # importance of each predictor
varImpPlot(fit)
MDSplot(rf, train.data$label)


#ROC AUc with train data

predictions=as.vector(fit$votes[,2])
pred=prediction(predictions,train.data$label)
predictions
perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC
perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot for RF")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

#predict with the model
mypredictions<-predict(fit, test.data, type="class")
plot(mypredictions,  test.data$label)

preds <- as.factor( predict(fit, newdata = test.data))
realized  <- as.factor(test.data$label)
confusionMatrix(preds, realized)
confusionMatrix(preds, realized, mode="prec_recall")
# tune parameters
trf <- tune.randomForest(label~., data = train.data, mtry = c(1: 10))
plot(trf)   
summary(trf)
trf2 <- tune.randomForest(label~., data = train.data, ntree = c(100, 250, 300, 400, 450, 500, 600, 800, 1000))
plot(trf2)
summary(trf2)
trf3 <- tune.randomForest(label~., data = train.data, nodesize = c(10:50))
plot(trf3)
summary(trf3)
# build the best model
rf <- randomForest(label ~., train.data, ntree=250, mtry=3, nodesize= 24,importance=TRUE, proximity=TRUE)
importance(rf) # importance of each predictor
varImpPlot(rf)
#predict with the model
mypredictions<-predict(rf, test.data, type="class")
plot(mypredictions,  test.data$label)
# metrics
preds <- as.factor( predict(rf, newdata = test.data))
realized  <- as.factor(test.data$label)
confusionMatrix(preds, realized)
confusionMatrix(preds, realized, mode="prec_recall")

pred <- predict(rf, newdata = test.data, type = "prob") 
t1.scores <- prediction(pred[,2], test.data$label)
t1.perf <- performance(t1.scores, "tpr", "fpr")

# Plot the ROC curve
plot(t1.perf, col = "green", lwd = 1.5, main = 'ROC Curve for RF optimized')
#AUC
t1.auc <- performance(t1.scores, "auc")
AUC=t1.auc@y.values[[1]]
AUC
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

