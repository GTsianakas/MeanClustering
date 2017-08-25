#not weighted murder clasifier (k mean cluster)

#load data
setwd("/home/giorgos/R")
#mnist in csv takesn from https://pjreddie.com/projects/mnist-in-csv/
testMnist <- read.csv("mnist_test.csv")
trainMnist <- read.csv("mnist_train.csv")

#seperate data and labels
testLabel <-testMnist[[1]]
trainLabel <- trainMnist[[1]]
testData <- testMnist[c(1:nrow(testMnist)),c(2:length(testMnist))]
trainData <- trainMnist[c(1:nrow(trainMnist)),c(2:length(trainMnist))]



#average matirx (training)

averages <- c()
for (i in (1:10)){
  averages = append(averages,as.integer(colMeans(subset(trainMnist, X5 == i-1, select = c(2:ncol(trainMnist))))))
}
avgMatrix <- matrix(averages,ncol = 784, byrow = T)


#get ideal n
# library(pracma)
#image(rot90(matrix(avgMatrix[1+1,],nrow = 28, ncol = 28),2))



correct <- 0
#test test data
stime <- proc.time()[3]
for(i in 1:nrow(testMnist)){
  if (i %% 100 == 0 ){
    print(paste(i, " out of ", nrow(testMnist), "  ", as.integer((i/nrow(testMnist) *100)), "%" ))
  }
  smallest <- 999999
  label <- 999
  for (j in 1:nrow(avgMatrix)){
    temp = dist(rbind(c(avgMatrix[j,]),c(testData[i,])))[1]
     if (temp < smallest){
      smallest = temp
      label = j-1
    }
  }
  if (label == testLabel[i]){
    correct = correct + 1
  }
}
print(paste("Elapsed Time",proc.time()[3]-stime))
print(paste(correct," correct"))
print(paste(correct/length(testLabel))*100,"% acuracy")

