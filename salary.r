salary_train<-read.csv(file.choose())
View(salary_train)
class(salary_train)
sum(is.na(salary_train))
class(salary_train$Salary)
salary_train$sal<-factor(salary_train$Salary)
class(salary_train$sal)
str(salary_train)
dim(salary_train)
salary_test<-read.csv(file.choose())
sum(is.na(salary_test))
View(salary_test)
dim(salary_test)
class(salary_test$Salary)
salary_test$sal1<-factor(salary_test$Salary)
class(salary_test$sal1)
str(salary_test)
class(salary_test)
##  Training a model on the data ----
library(e1071)
library(caret)
library(psych)
model<-naiveBayes(salary_train$Salary~.,data = salary_train)
model
sum(is.na(model))
##  Evaluating model performance ----
pred<-predict(model,salary_test)
pred
mean(pred==salary_test$Salary)
##0.8193227
confusionMatrix(pred,salary_test$Salary)
library(gmodels)
CrossTable(pred,salary_test$Salary)
## or##
CrossTable(pred,salary_test$Salary,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
