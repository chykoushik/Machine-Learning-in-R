getwd()

data <- read.csv("data.csv")
data
str(data)
summary(data)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


set.seed(3033)

help("createDataPartition")

#data spliting into training and test set
intrain <- createDataPartition(y = data$ASD, p= 0.7, list = FALSE)
train.data <- data[intrain,]
test.data <- data[-intrain,]

anyNA(data) #checking, is there any missing values?

#Cross-Validated (10 fold, repeated 3 times
cv <- trainControl(method = "repeatedcv", number = 10, repeats = 3)



#SVM Linear Kernel Model 
svm_Linear_fit <- train(ASD ~., data = train.data, kernel = "svmLinear", trControl=cv)
                  
#SVM Linear Kernel on train predict
svm_Linear_train_pred <- predict(svm_Linear_fit, train.data)

# Checking classification accuracy for train set
confusionMatrix(svm_Linear_train_pred, train.data$ASD)

#SVM Linear Kernal error on train
mean(svm_Linear_train_pred!=train.data$ASD)

#SVM Linear Kernal on test predict
svm_Linear_test_pred <- predict(svm_Linear_fit, test.data)

# Checking classification accuracy for test set
confusionMatrix(svm_Linear_test_pred, test.data$ASD)

#SVM Linear Kernal on test
mean(svm_Linear_test_pred!=test.data$ASD)



#SVM Linear Grid Kernel 
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)

#SVM Linear Grid Kernel Model
svm_Linear_Grid_fit <- train(ASD ~., data = train.data, method = "svmLinear", trControl=cv, tuneGrid = grid)
svm_Linear_Grid_fit 

plot(svm_Linear_Grid_fit) #plot svm linear grid

#SVM Linear Grid Kernel on train predict
svm_Linear_Grid_train_pred <- predict(svm_Linear_Grid_fit, train.data)

# Checking classification accuracy for train set
confusionMatrix(svm_Linear_Grid_train_pred, train.data$ASD)

#SVM Linear Grid Kernal error on train
mean(svm_Linear_Grid_train_pred!=train.data$ASD)

#SVM Linear Grid Kernal on test predict
svm_Linear_Grid_test_pred <- predict(svm_Linear_Grid_fit, test.data)

# Checking classification accuracy for test set
confusionMatrix(svm_Linear_Grid_test_pred, test.data$ASD)

#SVM Linear Grid Kernal on test
mean(svm_Linear_Grid_test_pred!=test.data$ASD)



#SVM Radial Kernal Model
set.seed(3233)
svm_Radial_fit <- train(ASD ~., data = train.data, kernel = "svmRadial", trControl=cv)

plot(svm_Radial_fit) #plot svm radial

#SVM Radial Kernel on train predict
svm_Radial_train_pred <- predict(svm_Radial_fit, train.data)

# Checking classification accuracy for train set
confusionMatrix(svm_Radial_train_pred, train.data$ASD)

#SVM Radial Kernal error on train
mean(svm_Radial_train_pred!=train.data$ASD)

#SVM Radial Kernal on test predict
svm_Radial_test_pred <- predict(svm_Radial_fit, test.data)

# Checking classification accuracy for test set
confusionMatrix(svm_Radial_test_pred, test.data$ASD)

#SVM Radial Kernal on test
mean(svm_Radial_test_pred!=test.data$ASD)



#SVM Radial Grid kernal
radial_grid <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                   1, 1.5, 2,5))
set.seed(3233)
svm_Radial_Grid_fit <- train(ASD ~., data = train.data, method = "svmRadial", trControl=cv, tuneGrid = radial_grid)
                          
plot(svm_Radial_Grid_fit) #plot svm radial grid

#SVM Radial Grid Kernel on train predict
svm_Radial_Grid_train_pred <- predict(svm_Radial_Grid_fit, train.data)

# Checking classification accuracy for train set
confusionMatrix(svm_Radial_Grid_train_pred, train.data$ASD)

#SVM Radial Grid Kernal error on train
mean(svm_Radial_Grid_train_pred!=train.data$ASD)

#SVM Radial Grid Kernal on test predict
svm_Radial_test_Grid_pred <- predict(svm_Radial_fit, test.data)

# Checking classification accuracy for test set
confusionMatrix(svm_Radial_test_Grid_pred, test.data$ASD)

#SVM Radial Grid Kernal on test
mean(svm_Radial_Grid_test_pred!=test.data$ASD)

library (e1071)



#SVM polnomial on train
svm_polynomial_fit <- train(ASD ~., data = train.data, kernel = "polynomial", trControl=cv)

plot(svm_polynomial_fit) #plot SVM Radial

#SVM polynomial on train predict
svm_polynomial_train_pred <- predict(svm_polynomial_fit, train.data)

# Checking classification accuracy for train set
confusionMatrix(svm_polynomial_train_pred, train.data$ASD) 

#SVM polynomial error on train
mean(svm_polynomial_train_pred!=train.data$ASD)

#SVM polynomial on test predict
svm_polynomial_test_pred <- predict(svm_polynomial_fit, test.data)

# Checking classification accuracy for test set
confusionMatrix(svm_polynomial_test_pred, test.data$ASD) 

#SVM polynomial error on test
mean(svm_polynomial_test_pred!=test.data$ASD)



#SVM Gaussian on train
svm_Gaussian_fit <- train(ASD ~., data = train.data, kernel = "gaussian", trControl=cv)

plot(svm_Gaussian_fit) #plot SVM Gaussian fit

#SVM Gaussian on train predict
svm_Gaussian_train_pred <- predict(svm_Gaussian_fit, train.data)

# Checking classification accuracy for train set
confusionMatrix(svm_Gaussian_train_pred, train.data$ASD) 

#SVM Gaussian error on train
mean(svm_Gaussian_train_pred!=train.data$ASD)

#SVM Gaussian on test predict
svm_Gaussian_test_pred <- predict(svm_Gaussian_fit, test.data)

# Checking classification accuracy for test set
confusionMatrix(svm_Gaussian_test_pred, test.data$ASD) 

#SVM Gaussian error on test
mean(svm_Gaussian_test_pred!=test.data$ASD)











