#GTID:sjiang98
#0. Data Preprocessing
train <- read.csv('~/Desktop/CSE6242/mnist_train.csv', header = FALSE)
test <- read.csv('~/Desktop/CSE6242/mnist_test.csv', header = FALSE)
test_df <- as.data.frame(test)
train_df <- as.data.frame(train)
test_t <- t(test_df)
train_t <- t(train_df)
dim(test_t)
dim(train_t)
#subsets of data
train_0_1 <- subset(train_t, train_t[,785]==0|train_t[,785]==1)
train_3_5 <- subset(train_t, train_t[,785]==3|train_t[,785]==5)
test_0_1 <- subset(test_t, test_t[,785]==0|test_t[,785]==1)
test_3_5 <- subset(test_t, test_t[,785]==3|test_t[,785]==5)
#corresponding true labels
true_label_train_0_1 <- train_0_1[,785]
true_label_test_0_1 <- test_0_1[,785]
true_label_train_3_5 <- train_3_5[,785]
true_label_test_3_5 <- test_3_5[,785]
#two sets of data
train_0_1 <- train_0_1[,1:784] #12665,784
train_3_5 <- train_3_5[,1:784] #11552,784
test_0_1 <- test_0_1[,1:784]#2115,784
test_3_5 <- test_3_5[,1:784]#1902,784
#sample four numbers
n1 <- sample(1:12665, 1)
n2 <- sample(1:11552, 1)
n3 <- sample(1:2115, 1)
n4 <- sample(1:1902, 1)
#matrix
data1 <- as.matrix(train_0_1[n1,])
data1 <- matrix (data1,nrow=28, ncol=28)#0
data2 <- as.matrix(train_3_5[n2,])
data2 <- matrix (data2,nrow=28, ncol=28)#3
data3 <- as.matrix(test_0_1[n3,])
data3 <- matrix (data3,nrow=28, ncol=28)#1
data4 <- as.matrix(test_3_5[n4,])
data4 <- matrix (data4,nrow=28, ncol=28)#5
#images
image(data1, col=gray.colors(256))
image(data2, col=gray.colors(256))
image(data3, col=gray.colors(256))
image(data4, col=gray.colors(256))

#2. Implementation
#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}
#Cost Function
cost <- function(theta, X, X_label)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-X_label*log(g)) - ((1-X_label)*log(1-g)))
  return(J)
}
#Intial theta, We will set theta parameters equal to zero initially and check the cost.
initial_theta_0_1 <- rep(0,ncol(train_0_1))
initial_theta_0_1
cost(initial_theta_0_1,train_0_1,true_label_train_0_1)
#You will find cost is 0.693 with initial parameters. Now, our objective is to minimize this cost and derive the optimal value of the thetas. For that we will use gradient descent optimization.
#gradient
grad <- function(X, y, theta){
  m = nrow(X)
  hx = sigmoid(X %*% theta)
  (1/m) * (t(X) %*% (hx - y))
}
#gradient decent
gradient_descent <- function (x, y, theta, iterations, alpha, epsilon) {
  m = nrow(x)
  temp = theta
  for(i in 1:iterations) {
    temp = temp - alpha * grad(x,y,theta)
    if (abs(norm(as.matrix(temp))-norm(as.matrix(theta))) < epsilon) {
      return (temp)
    }
    theta = temp
  }
  return (temp)
}

#3. Training
#train_0_1
#a predict function to calculate the predictions
initial_theta_0_1 <- rep(0,ncol(train_0_1))
X_0_1=train_0_1
y_0_1=true_label_train_0_1
initial_theta_0_1 <- gradient_descent(X_0_1, y_0_1, initial_theta_0_1, 100, 0.9, 0.001)
predicted_0_1 <- sigmoid(X_0_1 %*% initial_theta_0_1)
predicted_0_1
# an accuracy function to calculate accuracy
predicted_0_1[predicted_0_1 < 0.1] <- 0
predicted_0_1[predicted_0_1 > 0.1] <- 1
accuray <- sum(predicted_0_1 == true_label_train_0_1)/dim(train_0_1)[1]
accuray #99.25%
#train_3_5
initial_theta_3_5 <- rep(0,ncol(train_3_5))
X_3_5=train_3_5
y_3_5=true_label_train_3_5
y_3_5[y_3_5 == 3] <- 0
y_3_5[y_3_5 == 5] <- 1
initial_theta_3_5 <- gradient_descent(X_3_5, y_3_5, initial_theta_3_5, 100, 0.9, 0.001)
predicted_3_5 <- sigmoid(X_3_5 %*% initial_theta_3_5)
predicted_3_5
predicted_3_5[predicted_3_5 < 0.1] <- 0
predicted_3_5[predicted_3_5 > 0.1] <- 1
accuray_3_5 <- sum(predicted_3_5 == y_3_5)/dim(train_3_5)[1]
accuray_3_5 #91.05%
#test_3_5
initial_theta_3_5_test <- rep(0,ncol(test_3_5))
X_3_5_test=test_3_5
dim(test_3_5)
y_3_5_test=true_label_test_3_5
y_3_5_test[y_3_5_test == 3] <- 0
y_3_5_test[y_3_5_test == 5] <- 1
initial_theta_3_5_test <- gradient_descent(X_3_5_test, y_3_5_test, initial_theta_3_5_test, 100, 0.9, 0.001)
predicted_3_5_test <- sigmoid(X_3_5_test %*% initial_theta_3_5_test)
predicted_3_5_test
predicted_3_5_test[predicted_3_5_test < 0.1] <- 0
predicted_3_5_test[predicted_3_5_test > 0.1] <- 1
accuray_3_5_test<- sum(predicted_3_5_test == y_3_5_test)/dim(test_3_5)[1]
accuray_3_5_test #93.10%
#test_0_1
initial_theta_0_1_test <- rep(0,ncol(test_0_1))
X_0_1_test=test_0_1
y_0_1_test=true_label_test_0_1
initial_theta_0_1_test <- gradient_descent(X_0_1_test, y_0_1_test, initial_theta_0_1_test, 100, 0.9, 0.001)
predicted_0_1_test <- sigmoid(X_0_1_test %*% initial_theta_0_1_test)
predicted_0_1_test
predicted_0_1_test[predicted_0_1_test < 0.1] <- 0
predicted_0_1_test[predicted_0_1_test > 0.1] <- 1
accuray_0_1_test<- sum(predicted_0_1_test == y_0_1_test)/dim(test_0_1)[1]
accuray_0_1_test #99.76%

#sample 80% data for 10 times
tentimes <- list()
#subsets of data
train_0_1 <- subset(train_t, train_t[,785]==0|train_t[,785]==1)
train_3_5 <- subset(train_t, train_t[,785]==3|train_t[,785]==5)
test_0_1 <- subset(test_t, test_t[,785]==0|test_t[,785]==1)
test_3_5 <- subset(test_t, test_t[,785]==3|test_t[,785]==5)
#sample data
train_0_1 <- train_0_1[sample(nrow(train_0_1),nrow(train_0_1)*0.8),]
train_3_5 <- train_3_5[sample(nrow(train_3_5),nrow(train_3_5)*0.8),]
test_0_1 <- test_0_1[sample(nrow(test_0_1),nrow(test_0_1)*0.8),]
test_3_5 <- test_3_5[sample(nrow(test_3_5),nrow(test_3_5)*0.8),]
#corresponding true labels
true_label_train_0_1 <- train_0_1[,785]
true_label_test_0_1 <- test_0_1[,785]
true_label_train_3_5 <- train_3_5[,785]
true_label_test_3_5 <- test_3_5[,785]
#two sets of data
train_0_1 <- train_0_1[,1:784] #12665*0.8,784
train_3_5 <- train_3_5[,1:784] #11552*0.8,784
test_0_1 <- test_0_1[,1:784]#2115*0.8,784
test_3_5 <- test_3_5[,1:784]#1902*0.8,784
#train_0_1
#a predict function to calculate the predictions
initial_theta_0_1 <- rep(0,ncol(train_0_1))
X_0_1=train_0_1
y_0_1=true_label_train_0_1
initial_theta_0_1 <- gradient_descent(X_0_1, y_0_1, initial_theta_0_1, 100, 0.9, 0.001)
predicted_0_1 <- sigmoid(X_0_1 %*% initial_theta_0_1)
predicted_0_1
# an accuracy function to calculate accuracy
predicted_0_1[predicted_0_1 < 0.1] <- 0
predicted_0_1[predicted_0_1 > 0.1] <- 1
accuray_0_1 <- sum(predicted_0_1 == true_label_train_0_1)/dim(train_0_1)[1]
accuray_0_1 #99.25%
tentimes[1]=accuray_0_1
#train_3_5
initial_theta_3_5 <- rep(0,ncol(train_3_5))
X_3_5=train_3_5
y_3_5=true_label_train_3_5
y_3_5[y_3_5 == 3] <- 0
y_3_5[y_3_5 == 5] <- 1
initial_theta_3_5 <- gradient_descent(X_3_5, y_3_5, initial_theta_3_5, 100, 0.9, 0.001)
predicted_3_5 <- sigmoid(X_3_5 %*% initial_theta_3_5)
predicted_3_5
predicted_3_5[predicted_3_5 < 0.1] <- 0
predicted_3_5[predicted_3_5 > 0.1] <- 1
accuray_3_5 <- sum(predicted_3_5 == y_3_5)/dim(train_3_5)[1]
accuray_3_5 #91.05%
tentimes[2]=accuray_3_5
#test_3_5
initial_theta_3_5_test <- rep(0,ncol(test_3_5))
X_3_5_test=test_3_5
dim(test_3_5)
y_3_5_test=true_label_test_3_5
y_3_5_test[y_3_5_test == 3] <- 0
y_3_5_test[y_3_5_test == 5] <- 1
initial_theta_3_5_test <- gradient_descent(X_3_5_test, y_3_5_test, initial_theta_3_5_test, 100, 0.9, 0.001)
predicted_3_5_test <- sigmoid(X_3_5_test %*% initial_theta_3_5_test)
predicted_3_5_test
predicted_3_5_test[predicted_3_5_test < 0.1] <- 0
predicted_3_5_test[predicted_3_5_test > 0.1] <- 1
accuray_3_5_test<- sum(predicted_3_5_test == y_3_5_test)/dim(test_3_5)[1]
accuray_3_5_test #93.10%
tentimes[3]=accuray_3_5_test
#test_0_1
initial_theta_0_1_test <- rep(0,ncol(test_0_1))
X_0_1_test=test_0_1
y_0_1_test=true_label_test_0_1
initial_theta_0_1_test <- gradient_descent(X_0_1_test, y_0_1_test, initial_theta_0_1_test, 100, 0.9, 0.001)
predicted_0_1_test <- sigmoid(X_0_1_test %*% initial_theta_0_1_test)
predicted_0_1_test
predicted_0_1_test[predicted_0_1_test < 0.1] <- 0
predicted_0_1_test[predicted_0_1_test > 0.1] <- 1
accuray_0_1_test<- sum(predicted_0_1_test == y_0_1_test)/dim(test_0_1)[1]
accuray_0_1_test
tentimes[4]=accuray_0_1_test
tentimes

#4a and 4b
tentimes <- data.frame(matrix(ncol = 2, nrow = 0))
y <- c("1", "2")
colnames(tentimes) <- y
i=1
#subsets of data
train_3_5 <- subset(train_t, train_t[,785]==3|train_t[,785]==5)
test_3_5 <- subset(test_t, test_t[,785]==3|test_t[,785]==5)
#sample data
train_3_5 <- train_3_5[sample(nrow(train_3_5),nrow(train_3_5)*0.8),]
test_3_5 <- test_3_5[sample(nrow(test_3_5),nrow(test_3_5)*0.8),]
#corresponding true labels
true_label_train_3_5 <- train_3_5[,785]
true_label_test_3_5 <- test_3_5[,785]
#two sets of data
train_3_5 <- train_3_5[,1:784] #11552*0.8,784
test_3_5 <- test_3_5[,1:784]#1902*0.8,784
#train_3_5
initial_theta_3_5 <- rep(10,ncol(train_3_5))
X_3_5=train_3_5
y_3_5=true_label_train_3_5
y_3_5[y_3_5 == 3] <- 0
y_3_5[y_3_5 == 5] <- 1
initial_theta_3_5 <- gradient_descent(X_3_5, y_3_5, initial_theta_3_5, 100, 0.9, 0.001)
predicted_3_5 <- sigmoid(X_3_5 %*% initial_theta_3_5)
predicted_3_5
predicted_3_5[predicted_3_5 < 0.1] <- 0
predicted_3_5[predicted_3_5 > 0.1] <- 1
accuray_3_5 <- sum(predicted_3_5 == y_3_5)/dim(train_3_5)[1]
accuray_3_5 
tentimes[i,1]=accuray_3_5
#test_3_5
initial_theta_3_5_test <- rep(10,ncol(test_3_5))
X_3_5_test=test_3_5
dim(test_3_5)
y_3_5_test=true_label_test_3_5
y_3_5_test[y_3_5_test == 3] <- 0
y_3_5_test[y_3_5_test == 5] <- 1
initial_theta_3_5_test <- gradient_descent(X_3_5_test, y_3_5_test, initial_theta_3_5_test, 100, 0.9, 0.001)
predicted_3_5_test <- sigmoid(X_3_5_test %*% initial_theta_3_5_test)
predicted_3_5_test
predicted_3_5_test[predicted_3_5_test < 0.1] <- 0
predicted_3_5_test[predicted_3_5_test > 0.1] <- 1
accuray_3_5_test<- sum(predicted_3_5_test == y_3_5_test)/dim(test_3_5)[1]
accuray_3_5_test 
tentimes[i,2]=accuray_3_5_test
tentimes
write.xlsx(tentimes, "test_0.xlsx")

#5a and 5b (I used excel to store and visualize data in this case because my functions are not modular at the beginning. I would argue
#excel is a better choice in this case.)
df <- data.frame(matrix(ncol = 8, nrow = 0))
x <- c("1", "2", "3","4","5","6","7","8")
colnames(df) <- x
i=5
#subsets of data
train_0_1 <- subset(train_t, train_t[,785]==0|train_t[,785]==1)
train_3_5 <- subset(train_t, train_t[,785]==3|train_t[,785]==5)
test_0_1 <- subset(test_t, test_t[,785]==0|test_t[,785]==1)
test_3_5 <- subset(test_t, test_t[,785]==3|test_t[,785]==5)
#sample data
train_0_1 <- train_0_1[sample(nrow(train_0_1),nrow(train_0_1)*1),]
train_3_5 <- train_3_5[sample(nrow(train_3_5),nrow(train_3_5)*1),]
test_0_1 <- test_0_1[sample(nrow(test_0_1),nrow(test_0_1)*1),]
test_3_5 <- test_3_5[sample(nrow(test_3_5),nrow(test_3_5)*1),]
#corresponding true labels
true_label_train_0_1 <- train_0_1[,785]
true_label_test_0_1 <- test_0_1[,785]
true_label_train_3_5 <- train_3_5[,785]
true_label_test_3_5 <- test_3_5[,785]
#two sets of data
train_0_1 <- train_0_1[,1:784] #12665*0.8,784
train_3_5 <- train_3_5[,1:784] #11552*0.8,784
test_0_1 <- test_0_1[,1:784]#2115*0.8,784
test_3_5 <- test_3_5[,1:784]#1902*0.8,784
#train_0_1
#a predict function to calculate the predictions
initial_theta_0_1 <- rep(0,ncol(train_0_1))
X_0_1=train_0_1
y_0_1=true_label_train_0_1
initial_theta_0_1 <- gradient_descent(X_0_1, y_0_1, initial_theta_0_1, 100, 0.9, 0.001)
df[i,1] <- cost(initial_theta_0_1,X_0_1,y_0_1)
predicted_0_1 <- sigmoid(X_0_1 %*% initial_theta_0_1)
predicted_0_1
# an accuracy function to calculate accuracy
predicted_0_1[predicted_0_1 < 0.1] <- 0
predicted_0_1[predicted_0_1 > 0.1] <- 1
accuray_0_1 <- sum(predicted_0_1 == true_label_train_0_1)/dim(train_0_1)[1]
accuray_0_1 
df[i,2]=accuray_0_1
#train_3_5
initial_theta_3_5 <- rep(0,ncol(train_3_5))
X_3_5=train_3_5
y_3_5=true_label_train_3_5
y_3_5[y_3_5 == 3] <- 0
y_3_5[y_3_5 == 5] <- 1
initial_theta_3_5 <- gradient_descent(X_3_5, y_3_5, initial_theta_3_5, 100, 0.9, 0.001)
df[i,3] <- cost(initial_theta_3_5,X_3_5,y_3_5)
predicted_3_5 <- sigmoid(X_3_5 %*% initial_theta_3_5)
predicted_3_5
predicted_3_5[predicted_3_5 < 0.1] <- 0
predicted_3_5[predicted_3_5 > 0.1] <- 1
accuray_3_5 <- sum(predicted_3_5 == y_3_5)/dim(train_3_5)[1]
accuray_3_5 
df[i,4]=accuray_3_5
#test_3_5
initial_theta_3_5_test <- rep(0,ncol(test_3_5))
X_3_5_test=test_3_5
dim(test_3_5)
y_3_5_test=true_label_test_3_5
y_3_5_test[y_3_5_test == 3] <- 0
y_3_5_test[y_3_5_test == 5] <- 1
initial_theta_3_5_test <- gradient_descent(X_3_5_test, y_3_5_test, initial_theta_3_5_test, 100, 0.9, 0.001)
df[i,5] <- cost(initial_theta_3_5_test,X_3_5_test,y_3_5_test)
predicted_3_5_test <- sigmoid(X_3_5_test %*% initial_theta_3_5_test)
predicted_3_5_test
predicted_3_5_test[predicted_3_5_test < 0.1] <- 0
predicted_3_5_test[predicted_3_5_test > 0.1] <- 1
accuray_3_5_test<- sum(predicted_3_5_test == y_3_5_test)/dim(test_3_5)[1]
accuray_3_5_test 
df[i,6]=accuray_3_5_test
#test_0_1
initial_theta_0_1_test <- rep(0,ncol(test_0_1))
X_0_1_test=test_0_1
y_0_1_test=true_label_test_0_1
initial_theta_0_1_test <- gradient_descent(X_0_1_test, y_0_1_test, initial_theta_0_1_test, 100, 0.9, 0.001)
df[i,7] <- cost(initial_theta_0_1_test,X_0_1_test,y_0_1_test)
predicted_0_1_test <- sigmoid(X_0_1_test %*% initial_theta_0_1_test)
predicted_0_1_test
predicted_0_1_test[predicted_0_1_test < 0.1] <- 0
predicted_0_1_test[predicted_0_1_test > 0.1] <- 1
accuray_0_1_test<- sum(predicted_0_1_test == y_0_1_test)/dim(test_0_1)[1]
accuray_0_1_test
df[i,8]=accuray_0_1_test

df
library(xlsx)
write.xlsx(df, "test_1.xlsx")
accuray_0_1_test #99.76%