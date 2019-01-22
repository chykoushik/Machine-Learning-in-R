getwd()

library(MASS) 
library(neuralnet)
library(readr)


data <- read.csv("data.csv")
data

str(data)
 
head(data)

#converting the attribute value
data[, 12] <- as.numeric(data[, 12] == "m")
data[, 13] <- as.numeric(data[, 13] == "Others")


data

#converting into numeric
data$ethnicity<-as.numeric(data$ethnicity) 
data$country<-as.numeric(data$country)
data$age_desc<-as.numeric(data$age_desc)
data$relation<-as.numeric(data$relation)
data$gender<-as.numeric(data$gender)
data$jundice<-as.numeric(data$jundice)
data$family_member_with_PDD<-as.numeric(data$family_member_with_PDD)
data$used_qn_before<-as.numeric(data$used_qn_before)
data$ASD <- as.numeric(data$ASD == "YES")

str(data)

#remove a attribute
data <- data[, -c(16)]

#converting into numeric
data$Q1<-as.numeric(data$Q1)
data$Q2<-as.numeric(data$Q2)
data$Q3<-as.numeric(data$Q2)
data$Q4<-as.numeric(data$Q4)
data$Q5<-as.numeric(data$Q5)
data$Q6<-as.numeric(data$Q6)
data$Q7<-as.numeric(data$Q7)
data$Q8<-as.numeric(data$Q8)
data$Q9<-as.numeric(data$Q9)
data$Q10<-as.numeric(data$Q10)
data$age<-as.numeric(data$age)
data$result<-as.numeric(data$result)


#change the column name
colnames(data)[1] <- "V1"
colnames(data)[2] <- "V2"
colnames(data)[3] <- "V3"
colnames(data)[4] <- "V4"
colnames(data)[5] <- "V5"
colnames(data)[6] <- "V6"
colnames(data)[7] <- "V7"
colnames(data)[8] <- "V8"
colnames(data)[9] <- "V9"
colnames(data)[10] <- "V10"
colnames(data)[11] <- "V11"
colnames(data)[12] <- "V12"
colnames(data)[13] <- "V13"
colnames(data)[14] <- "V14"
colnames(data)[15] <- "V15"
colnames(data)[16] <- "V16"
colnames(data)[17] <- "V17"
colnames(data)[18] <- "V18"
colnames(data)[19] <- "V19"



data[1, ]
data

str(data)

#creatng train and test set
intrain <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[intrain,]
test <- data[-intrain,]

#lm model
lm.fit <- glm(ASD~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$ASD)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_scaled <- scaled[intrain,]
test_scaled <- scaled[-intrain,]

library(neuralnet)
n <- names(train_scaled)
f <- as.formula(paste("ASD ~", paste(n[!n %in% "ASD"], collapse = " + ")))
neural.net <- neuralnet(f,data=train_scaled,hidden=c(5,3),linear.output=T)

plot(neural.net)

pr.nn <- compute(neural.net,test_scaled[,1:19])

pr.nn <- pr.nn$net.result*(max(data$ASD)-min(data$ASD))+min(data$ASD)
test.r <- (test_scaled$ASD)*(max(data$ASD)-min(data$ASD))+min(data$ASD)

MSE.nn <- sum((test.r - pr.nn)^2)/nrow(test_scaled)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))

plot(test$ASD,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$ASD,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


plot(test$ASD,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$ASD,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottom',legend=c('NN','LM'),pch=18,col=c('red','blue'))
