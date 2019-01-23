#load the dataset
data <- read.csv("data.csv")
data
str(data)
summary(data)

help("createDataPartition")

#data spliting into training and test set
intrain <- createDataPartition(y = data$ASD, p= 0.7, list = FALSE)
train.data <- data[intrain,]
test.data <- data[-intrain,]

#-------4(b)-----------

library (randomForest)

rf.fit <- randomForest(ASD ~., importance =TRUE, data = train.data)

#predicting on train set
pred.train <- predict(rf.fit, newdata = train.data)

# Checking classification accuracy for train set
confusionMatrix(pred.train, train.data$ASD) 

#train set misclassification error
mean(pred.train != train.data$ASD)

#predicting on test set
pred.test <- predict(rf.fit, newdata = test.data)

# Checking classification accuracy for test set
confusionMatrix(pred.test, test.data$ASD) 

#test set misclassification error
mean(pred.test != test.data$ASD)