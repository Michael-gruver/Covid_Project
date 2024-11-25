library(randomForest)
library(MASS)


#Importing Training data

#Imported from a .csv file, this file has the question description row deleted from the file
training_csv = read.table("Training_data_relabel-1.csv", header=T,sep = ",")
is.data.frame(training_csv)
summary(training_csv)

training_data = training_csv[,1:152]
is.data.frame(training_data)
summary(training_data)

#cleaning out NA value rows
training_data <- na.omit(training_data) 
training_data$Y <- factor(training_data$Y)

## Splitting training data into seperate training and testing sets
set.seed(1)
train = sample(1:nrow(training_data), nrow(training_data)*0.8)
data.train=training_data[train,]
data.test=training_data[-train,]

nrow(data.test)


###### Finding Optimal Ntreee Value ##############
tree_errors <- rep(1,200)

for (i in 1:200) {
  bag.data=randomForest(Y~.,data=data.train, mtry=sqrt(152), ntree = i, importance = TRUE)
  yhat.bag=predict(bag.data,newdata=training_data[-train ,])
  tree_errors[i] <- sum(abs((as.numeric(yhat.bag) - as.numeric(data.test$Y))))
}
min(tree_errors)

plot(tree_errors,main = "Error vs. ntree", xlab= "ntree", ylab = "Error", type="b", xlim=c(5,200))
treee <- which.min(tree_errors)
treee

###### Finding optimal mtry value ###############
mtry_errors <- rep(1,151)

for (j in 1:151) {
  bag.data=randomForest(Y~.,data=data.train, mtry=j, ntree = treee, importance = TRUE)
  yhat.bag=predict(bag.data,newdata=training_data[-train ,])
  mtry_errors[j] <- sum(abs((as.numeric(yhat.bag) - as.numeric(data.test$Y))))
}
plot(mtry_errors,main = "Error vs. mtry", xlab= "mtry", ylab = "Error", type="b", xlim=c(0,151))
min(mtry_errors)
tryy <- which.min(mtry_errors)
tryy
########## Optimal Model ########
set.seed(30)
bag.data=randomForest(Y~.,data=data.train, mtry=tryy, ntree = treee, importance = TRUE)
yhat.bag=predict(bag.data,newdata=training_data[-train ,])
conf_matrix <- table(yhat.bag, data.test$Y)
print(conf_matrix)
error <- sum(abs((as.numeric(yhat.bag) - as.numeric(data.test$Y))))
cat("Final Error", error)


######## Phase 2 ###############
####Input data from a .csv file ############
testing_csv = read.table("Test_data-1.csv", header=T,sep = ",")
is.data.frame(testing_csv)
summary(testing_csv)

testing_data = testing_csv[,1:152]
is.data.frame(training_data)
summary(testing_data)

## Getting rid of rows with possible NA values
testing_data <- na.omit(testing_data) 
testing_data$Y <- factor(testing_data$Y)

nrow(testing_data)
##### Initial Final model with evaluation on test data ##############
set.seed(100)
bag.data=randomForest(Y~.,data=data.train, mtry=tryy, ntree = treee, importance = TRUE)
yhat.bag=predict(bag.data,newdata=testing_data)
conf_matrix <- table(yhat.bag, testing_data$Y)
print(conf_matrix)
error <- sum(abs((as.numeric(yhat.bag) - as.numeric(testing_data$Y))))
cat("Final Error", error)


###### Improved Model utilizing all training data points #######
set.seed(100)
bag.data=randomForest(Y~.,data=training_data, mtry=tryy, ntree = treee, importance = TRUE)
yhat.bag=predict(bag.data,newdata=testing_data)
conf_matrix <- table(yhat.bag, testing_data$Y)
print(conf_matrix)
error <- sum(abs((as.numeric(yhat.bag) - as.numeric(testing_data$Y))))
cat("Improved Final Error", error)

