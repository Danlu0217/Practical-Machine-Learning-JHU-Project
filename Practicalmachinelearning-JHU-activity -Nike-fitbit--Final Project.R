download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="training.csv")

file1<-read.csv("training.csv",na.strings=c("NA","#DIV/0!", ""))
summary(file1$classe)
dim(file1)
library(caret)
library(tidyverse)
library(tidyr)
library(randomForest)
#Delete columns with all missing values
file2<-file1[,colSums(is.na(file1))==0]
#New dataframe dimensions
dim(file2)
# Rplace NA in each column with median of that column.
for(i in 1:ncol(file2))
   {
  file2[is.na(file2[,i]), i] <- median(as.numeric(file2[,i]), na.rm = TRUE)
}
# Do some exploratory study on the data set
summary(file2)
# Columns 1:7 have nothing to do with the model, so we need to subset the data.
file3<-file2[8:ncol(file2)]
set.seed(11)
sample<-createDataPartition(file3$classe,p=0.7,list=FALSE)
train<-file3[sample,]
test<-file3[-sample,]
# Train the model with "lda" method
fit.lda <- train(classe~., data = train, method = "lda")
# See the accuracy of the model
pre.lda<-confusionMatrix(test$classe,predict(fit.lda,test))
# Train the model with "randomForest" method
fit.rf <- randomForest(classe~., data = train, method = "class")
acc.rf<-confusionMatrix(test$classe,predict(fit.rf,test))
# Try decision tree method
library(rpart)
fit.tree<-rpart(classe~.,data=train,method = "class")
pre.tree<-predict(fit.tree,test,type ="class")
#Accuracy
acc.tree<-confusionMatrix(test$classe,pre.tree)

