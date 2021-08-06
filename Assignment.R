install.packages("naivebayes")
library(naivebayes)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

#loading of testing and training data
test <- read.csv(file.choose())
train <- read.csv(file.choose())

#Accessing the data
View(train)
table(train$occupation)

#Visualising the data
train %>%
ggplot(aes(x=Salary,y=occupation,fill=Salary))+
  geom_boxplot()+
  ggtitle("Boxplot")

train %>%
ggplot(aes(x=occupation,fill=Salary))+
  geom_density(alpha=0.8,colours='black',n=2)+
  ggtitle("Density")


#Formation of Naive Bayes Model
model <- naive_bayes(Salary~.,data = train,usekernel = T)
plot(model)
summary(model)

#Probablity Calculation
p <- predict(model,train,type = "prob")
View(cbind(p,train$occupation,train$Salary,train$native,train$workclass))

#Prediction of Training Data
pred <- predict(model,train)
tab1 <- table(pred,train$Salary)      
tab1
sum(diag(tab1))/sum(tab1) #80% accuracy

#Prediction of Testing Data
pred1 <- predict(model,test)
tab2 <- table(pred1,test$Salary)
tab2
sum(diag(tab2))/sum(tab2)# 0.8081673 or 80% accuracy 
