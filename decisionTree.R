#required library
install.packages("rpart")
library(rpart)
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
library(lattice)
library(ggplot2)
library(caret)
#dataset
dataset <- read.csv("C:\\Users\\sen\\Desktop\\Access-log-Prediction\\accesslogml_trainingNBD.csv")
#removing NA values
dataset <- na.omit(dataset)
# fit into decision tree
fit <- rpart(result~  uni + Workstation + Dep + Exp+ filesize + filetype ,
             method="class", data=dataset)

#Print the result
printcp(fit)
#plot the cross-validation result
plotcp(fit) 

#detail summary
summary(fit)
par(mfrow=c(1,1))

# plot the decision tree 
plot(fit, uniform=TRUE, 
     main="Decision Tree")

#labelling the tree
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#pruning the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Access Result")
#labelling the tree
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#calculate the prediction
pred = predict(pfit, type="class")
#result of prediction
table(pred)

table(pred,dataset$result)

#Confusion Matrix and statistics
c<-confusionMatrix(pred,as.factor(dataset$result),mode = "prec_recall", positive="1")
c


