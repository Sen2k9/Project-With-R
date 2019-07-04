
#packages for neural network
install.packages("nnet")
library(nnet)

install.packages("neuralnet")
library(neuralnet)

# reads the Data (change the directory name as where you saved your data)
dataset <- read.csv("C:\\Users\\sen\\Desktop\\Access-log-Prediction\\accesslogml_trainingNBD.csv", header = T, sep = ",")

#randomly sample row number for training data
trainDataInds <- sample(1:2250,1150, replace = FALSE)


# pulls the corresponding data from the dataset
trainData <- dataset[trainDataInds, ]

#separate the groundTruth
groundTruth <- class.ind(dataset$result)

#corresponding data for ground truth
result <- groundTruth[trainDataInds]
str(result)

#removing the unnecessary columns
dataForTrain = as.data.frame(cbind(result, trainData[, c("Salary","Workstation","Dep","Exp","filesize","filetype")]))
str(dataForTrain)

#removing NA values
dataForTrain <- na.omit(dataForTrain)

#train the model with training data
nn <- neuralnet(result ~ Salary+Workstation+Dep+Exp+filesize+filetype, data = dataForTrain,hidden = c(3,2), linear.output = FALSE, threshold=0.01)

#plot the neuralnetwork	
plot(nn)

# selects the test dataset as the complementary points of the training set
testData <- dataset[-trainDataInds, ]

# get the list of predictions  for the test dataset
nnoutputs <- compute(nn, testData[,c("Salary","Workstation","Dep","Exp","filesize","filetype")
                                  ])$net.result

nnoutputs

# define a function to return a list containing rounded results
roundResults <- function(predictions) {
  
  roundedPreds <- list()
  
  # rounds each prediction to 0 if it is less than 0.5 else 1
  for(i in 1:length(predictions)){
    
    if(predictions[i]<0.5){
      roundedPreds[i] <- round(predictions[i], digits = 0)
    }else  {
      predictions[i]<- 1
      roundedPreds[i] <- predictions[i]
    }
    
  } 
  
  roundedPreds <- unlist(roundedPreds)
  
  return(roundedPreds)
} #  end "predictions" def'n

roundedPreds <- roundResults(nnoutputs)
str(roundedPreds)
str(testData[, 13])


# create a dataframe adding true labels of each data point
predictedVsTrue <- data.frame(roundedPreds, testData[, 13])

# calculate the accuracy
accuracy <- 0
for(i in 1:nrow(predictedVsTrue)){
  if(predictedVsTrue[i, 1] == predictedVsTrue[i, 2]){
    accuracy <- accuracy + 1
  } else if(!(predictedVsTrue[i, 2] == "1") & !(predictedVsTrue[i, 1] == "1")){
    accuracy <- accuracy + 1
  }
}

#Finally the accuracy

accuracy <- accuracy/nrow(predictedVsTrue) * 100
print(paste("The accuracy of the model is :", accuracy, "%", sep = " "))

