rm(list=ls())

setwd(getwd())

#install libraries to run this code and it might require r studio restart


# install(rpart)
# install(rattle)
# install(rpart.plot)
# install(RColorBrewer)
# install(caret)
# install("ROSE")
# install(e1071)
# install(randomForest)
# install(partykit)
# install(ggplot2)
# install(pROC)

##Load data ###

Consumercomplaintsdata=read.csv("Consumer_Complaints.csv",check.names=FALSE)
Consumercomplaintsdata <- Consumercomplaintsdata[!(Consumercomplaintsdata$Tags == ""), ]
Consumercomplaintsdata <- Consumercomplaintsdata[!(Consumercomplaintsdata$State == ""), ]

## remove unwanted characters from data

as.data.frame(sapply(Consumercomplaintsdata, function(x) gsub("\"", "", x)))

## Mapp the column values to specific categories

columnmappings <- read.csv("mapping.csv",check.names=FALSE)


## To remove the NA cases from the class label

Consumercomplaintsdata[Consumercomplaintsdata == "N/A"] <- "na"


completeFunction <- function(data,columnval) {
  completeVec <- complete.cases(data[,columnval])
  return(data[completeVec, ])
}

formatted_consumerdata <-completeFunction(Consumercomplaintsdata)

## create matrices

consumerdata_matrix <-  as.matrix(formatted_consumerdata)
colummapping_matrix <- as.matrix(columnmappings)

# Mapp the column values to attribute
for (i in 1:nrow(consumerdata_matrix)) #nrow(consumerdata_matrix)
{
  for (j in 1:nrow(colummapping_matrix)) #nrow(colummapping_matrix)
  {
    
    
    if(consumerdata_matrix[i,4] ==  colummapping_matrix[j,1])
    {

      consumerdata_matrix[i,4] <- colummapping_matrix[j,2]


    }

    
    if(consumerdata_matrix[i,9] == colummapping_matrix[j,3])
    {
      consumerdata_matrix[i,9] <- colummapping_matrix[j,4]
     
 
    }
    
    
  }
  
}

### creating data frame#####
consumerdataframe = as.data.frame(consumerdata_matrix)

### Divide the data to training and testing

set.seed(42)
id <-sample(2,nrow(consumerdataframe),prob=c(0.7,0.3),replace=TRUE)
consumertrain_data <- consumerdataframe[id==1,]
consumertest_data <-  consumerdataframe[id==2,]
#division_index <- sample(1:nrow(consumerdataframe),size=0.7 * nrow(consumerdataframe))

#consumertrain_data<- consumerdataframe[division_index,]
#consumertest_data <- consumerdataframe[-division_index,]

###Data conversion to numeric
consumertrain_data$Company <-  as.numeric(consumertrain_data$Company)
consumertrain_data$Issue <- as.numeric(consumertrain_data$Issue)

consumertest_data$Company <- as.numeric(consumertest_data$Company)
consumertest_data$Issue <- as.numeric(consumertest_data$Issue)

#count the yes and no lable in Training data

prop.table(table(consumertrain_data$Consumerdisputed))
###Decision tree implementation using Rpart

library(rpart)
Consumerdatatree<-rpart(Consumerdisputed  ~ Product+Issue+Company+State+Tags,data=consumertrain_data,
            control=rpart.control(minsplit=50, minbucket=round(50/3), cp=0.00018),
            method="class")

#CP value should be such that root node error is minimum, is considered from below expression
cp<-Consumerdatatree$cptable[which.min(Consumerdatatree$cptable[,"xerror"])]
printcp(Consumerdatatree)
plotcp(Consumerdatatree)
plot.new()
plot(Consumerdatatree,uniform=TRUE, main="ConsumerComplaints Data Classification Tree")
text(Consumerdatatree,use.n = TRUE,pretty=0)


#fancy tree


library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(Consumerdatatree,main="ConsumerComplaints Data Classification Tree")

#test data prediction
prop.table(table(consumertest_data$Consumerdisputed))
Consumertree_Pred <- predict(Consumerdatatree,newdata = consumertest_data,type="class")
###install(caret)
library(caret)
cm<-confusionMatrix(table(Consumertree_Pred,consumertest_data$Consumerdisputed)) #0.8067
fourfoldplot(cm$table,color = c("#CC6666", "#99CC99"))
##### from prop.table function we know that dataset has imbalance data to normalise it we are using rose library####
library("ROSE")

Consumertree_Pred<-predict(Consumerdatatree,consumertest_data)
accuracy.meas(consumertest_data$Consumerdisputed, Consumertree_Pred[,2])
roc.curve(consumertest_data$Consumerdisputed, Consumertree_Pred[,2])


###Consumer data over sampling
Consumerdata_balanced_over <- ovun.sample(Consumerdisputed  ~ Product+Issue+Company+State+Tags, data = consumertrain_data, method = "over",N = 48528,seed=42)$data
table(Consumerdata_balanced_over$Consumerdisputed)

#tree for over sampled data
Consumetree.over <- rpart(Consumerdisputed  ~ Product+Issue+Company+State+Tags,data = Consumerdata_balanced_over,
                   control=rpart.control(minsplit=100, minbucket=round(100/3), cp=0.013),
                   method="class")

plotcp(Consumetree.over)
printcp(Consumetree.over)

plot(Consumerdatatree,uniform=TRUE)
text(Consumerdatatree,use.n = TRUE,pretty=0)

fancyRpartPlot(Consumetree.over)#, uniform=TRUE,main="Pruned Classification Tree")

#Prediction, confusion matrix and roc for over sample data
ConsumerTreeOversample_Pred <- predict(Consumetree.over,newdata = consumertest_data,type = "class") #type = "class"
cm<-confusionMatrix(table(ConsumerTreeOversample_Pred,consumertest_data$Consumerdisputed))
fourfoldplot(cm$table,color = c("#CC6666", "#99CC99"))
ConsumerTreeOversample_Pred <- predict(Consumetree.over,newdata = consumertest_data) 
roc.curve(consumertest_data$Consumerdisputed, ConsumerTreeOversample_Pred[,2])


### under sampling 207654
Consumerdata_balanced_under <- ovun.sample(Consumerdisputed ~ Product+Issue+Company+State+Tags, data = consumertrain_data, method = "under",N=11948,seed = 42)$data
table(Consumerdata_balanced_under$Consumerdisputed)
table(consumertrain_data$Consumerdisputed)

#Consumer data under sampled

consumertree.under <- rpart(Consumerdisputed  ~ Product+Issue+Company+State+Tags,data = Consumerdata_balanced_under,
                    control=rpart.control(minsplit=100, minbucket=round(100/3),cp=0.01),
                    method="class" )

#tree for under sampled data
cp<-consumertree.under$cptable[which.min(consumertree.under$cptable[,"xerror"])]

plotcp(consumertree.under)
printcp(consumertree.under)
fancyRpartPlot(consumertree.under)

#Prediction,confusion matrix and roc for under sampled data
ConsumerTreeUndersample_Pred <- predict(consumertree.under,newdata = consumertest_data,type = "class")#,type = "class")#,type = "class")
library(caret)
confusionMatrix(table(ConsumerTreeUndersample_Pred,consumertest_data$Consumerdisputed))
fourfoldplot(cm$table,color = c("#CC6666", "#99CC99"))
ConsumerTreeUndersample_Pred <- predict(consumertree.under,newdata = consumertest_data)
roc.curve(consumertest_data$Consumerdisputed, ConsumerTreeUndersample_Pred[,2],main="ROC Cuver for Under sample")

##Consumer data sampled for both under and over 
Consumerdata_balanced_both <- ovun.sample(Consumerdisputed ~ Product+Issue+Company+State+Tags, data = consumertrain_data, method = "both", p=0.5, seed = 42)$data
table(Consumerdata_balanced_both$Consumerdisputed)

#tree for sampled data
consumertree.both <- rpart(Consumerdisputed  ~ Product+Issue+Company+State+Tags,data = Consumerdata_balanced_both,
                   control=rpart.control(minsplit=100, minbucket=round(100/3), cp=0.013), #0.013
                   method="class")
#cp<-consumertree.under$cptable[which.min(consumertree.under$cptable[,"xerror"])]
plotcp(consumertree.both)
printcp(consumertree.both)
fancyRpartPlot(consumertree.both)

#prediction, cofusion matrix and roc for the balanced consuemr data
ConsumertreeBothsample_Pred <- predict(consumertree.both,newdata = consumertest_data,type="class")
confusionMatrix(table(ConsumertreeBothsample_Pred,consumertest_data$Consumerdisputed))
fourfoldplot(cm$table,color = c("#CC6666", "#99CC99"))
ConsumertreeBothsample_Pred <- predict(consumertree.both,newdata = consumertest_data)
roc.curve(consumertest_data$Consumerdisputed, ConsumertreeBothsample_Pred[,2])


#######Naive bayes for consumer data###



library(e1071)
model <- naiveBayes(Consumerdisputed ~ Product+Issue+Company+State+Tags, data = Consumerdata_balanced_both)
class(model)
summary(model)
print(model)


#Prediction and confusion matrix for consumer data

ConsumerNaivepred<-predict(model,consumertest_data,type=c("class"))
library(caret)
confusionMatrix(table(ConsumerNaivepred,consumertest_data$Consumerdisputed)) #0.8067
fourfoldplot(cm$table,color = c("#CC6666", "#99CC99"))


####random forest for consumer data

library(randomForest)

ConsumerData_RandomForestTree <- randomForest(Consumerdisputed  ~ Product+Tags+State+Issue+Company,data = Consumerdata_balanced_both)

#important attributes
importance(ConsumerData_RandomForestTree)
varImpPlot(ConsumerData_RandomForestTree)

# Prediction and confusion mastrix
ConsumerRandomForestPred <-  predict(ConsumerData_RandomForestTree,consumertest_data,type="class")
library(caret)
cm<-confusionMatrix(table(ConsumerRandomForestPred,consumertest_data$Consumerdisputed)) #0.8067
fourfoldplot(cm$table,color = c("#CC6666", "#99CC99"))
###need to do plotting





### cTREEE plot for consumer data


library(partykit)
#trainq <- subset(training_data$Product, !is.na(consumertrain_data$Consumerdisputed))
ConsumerTree_CT <- ctree(Consumerdisputed ~ Product+Issue+Company+State+Tags,
            control= ctree_control(teststat=c("quad","max"),testtype=c("Bonferroni", "Univariate", "Teststatistic"),
                                   mincriterion=0.99,minsplit=40L,minbucket=8L,maxsurrogate = 3),data=Consumerdata_balanced_both)


plot.new()
plot(ConsumerTree_CT)
mtext(text = "Consumerdisputed", side = 2, srt = 90, padj = -4, adj = 0)  
print(ConsumerTree_CT)

### gplot


library(ggplot2)
qplot(State, Product,data=consumertrain_data, colour=Consumerdisputed, size=I(4))
qplot(State, Tags, data=Consumerdata_balanced_both, colour=Consumerdisputed, size=I(4))


###ROC curves for over,under and both sampling.


#AUC Oversampling
roc.curve(consumertest_data$Consumerdisputed, ConsumerTreeOversample_Pred[,2])
#Area under the curve (AUC): 0.798

#AUC Undersampling
roc.curve(consumertest_data$Consumerdisputed, ConsumerTreeUndersample_Pred[,2])
#Area under the curve (AUC): 0.867

#AUC Both
rocobj<-roc.curve(consumertest_data$Consumerdisputed, ConsumertreeBothsample_Pred[,2])
#Area under the curve (AUC): 0.798


library(pROC)

# Create a basic roc object

rocobj1 <- roc(consumertest_data$Consumerdisputed,ConsumerTreeUndersample_Pred[,2])
rocobj2 <-roc(consumertest_data$Consumerdisputed,ConsumerTreeOversample_Pred[,2])
rocobj3 <-roc(consumertest_data$Consumerdisputed,ConsumertreeBothsample_Pred[,2])

#ggroc(list(ConsumerTreeUndersample=rocobj, ConsumerTreeOversample=rocobj2,ConsumerTreeBothsample=rocobj3), aes="linetype", colour = 'RED')


# plots precision and recall
coords(rocobj1, "best", ret = c("threshold", "sensitivity", "specificity", "precision", "recall"))
plot(precision ~ recall, t(coords(rocobj1, "all", ret = c("recall", "precision"))), type="l", main = "PR plot of UnderSampleConsumerdata")

coords(rocobj2, "best", ret = c("threshold", "sensitivity", "specificity", "precision", "recall"))
plot(precision ~ recall, t(coords(rocobj2, "all", ret = c("recall", "precision"))), type="l", main = "PR plot of OverSampleConsumerdata")

coords(rocobj3, "best", ret = c("threshold", "sensitivity", "specificity", "precision", "recall"))
plot(precision ~ recall, t(coords(rocobj3, "all", ret = c("recall", "precision"))), type="l", main = "PR plot of OverSampleConsumerdata")



# plots sensitivity and specificity
coords(rocobj1, "best", ret = c("threshold", "sensitivity", "specificity", "precision", "recall"))
plot(sensitivity ~ specificity, t(coords(rocobj1, "all", ret = c("sensitivity", "specificity"))), type="l", main = "PR Plot Of UnderSampleConsumerdata")

coords(rocobj2, "best", ret = c("threshold", "sensitivity", "specificity", "precision", "recall"))
plot(sensitivity ~ specificity, t(coords(rocobj2, "all", ret = c("sensitivity", "specificity"))), type="l", main = "PR plot Of OverSampleConsumerdata")

coords(rocobj3, "best", ret = c("threshold", "sensitivity", "specificity", "precision", "recall"))
plot(sensitivity ~ specificity, t(coords(rocobj3, "all", ret = c("sensitivity", "specificity"))), type="l", main = "PR Plot of OverSampleConsumerdata", col = "blue")

##roc Plots combined

plot(rocobj, col = "red")
par(new = TRUE)
plot(rocobj2, col = "blue")
par(new = TRUE)
plot(rocobj3, col = "green", xaxt = "n", yaxt = "n")
legend("right", legend = c("ROC UnderSample Data", "ROC OverSampleData","ROC BothSample Data"), col = c("red","blue", "green"), lty = 1, title="ROC Curves")



