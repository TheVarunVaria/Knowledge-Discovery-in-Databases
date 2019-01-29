setwd("/Users/thevarunvaria/Downloads")
mydata <- read.csv("ground_truth_one_row_per_article.csv", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))


str(mydata)
summary(mydata)

mydata$Surprise <- (mydata$suprise_r1 + mydata$suprise_r2 + mydata$suprise_r3)/3



mydata$main_category<-NULL

#Removing coloumns with more levels

mydata$category_1<-NULL
#If we minimise the number of categories, We can use this as a feature set

mydata$category_2<-NULL
mydata$category_3<-NULL
mydata$category_4<-NULL



mydata$c2_familiarity_r1<-NULL
mydata$c2_familiarity_r2<-NULL
mydata$c2_familiarity_r3<-NULL
mydata$c3_familiarity_r1<-NULL
mydata$c3_familiarity_r2<-NULL
mydata$c3_familiarity_r3<-NULL
mydata$c4_familiarity_r1<-NULL
mydata$c4_familiarity_r2<-NULL
mydata$c4_familiarity_r3<-NULL
mydata$article_id<-NULL
mydata$link<-NULL
mydata$title<-NULL


#1,2 - Not Surprised... 4,5 - Surprised
mydata$Surprise <- ifelse(mydata$Surprise <= 3 ,0,1)


#Using following columns as class for our model
mydata$suprise_r1 <- as.factor(mydata$suprise_r1)
mydata$suprise_r2 <- as.factor(mydata$suprise_r2)
mydata$suprise_r3 <- as.factor(mydata$suprise_r3)
mydata$like_r1 <- as.factor(mydata$like_r1)
mydata$like_r2 <- as.factor(mydata$like_r2)
mydata$like_r3 <- as.factor(mydata$like_r3)
mydata$c1_familiarity_r1 <- as.factor(mydata$c1_familiarity_r1)
mydata$c1_familiarity_r2 <- as.factor(mydata$c1_familiarity_r2)
mydata$c1_familiarity_r3 <- as.factor(mydata$c1_familiarity_r3)



summary(mydata)

#Using Surprise as a factor
mydata$Surprise <- as.factor(mydata$Surprise)


summary(mydata)

#Dividing the data in training data and Test Data
set.seed(100)
dataframe<-sample(1:nrow(mydata),0.8* nrow(mydata))
TrainingData <-mydata[dataframe, ]
TestData <-mydata[-dataframe, ]
nrow(TrainingData)
nrow(TestData)

summary(TrainingData)


#Implementing the model
library(randomForest)
fgl.rf <- randomForest(Surprise ~ ., data = TrainingData,mtry = 2, importance = TRUE)
fgl.rf
fgl.rf$importance

fgl.rf <- randomForest(Surprise ~ ., data = TrainingData,mtry = 2, importance = TRUE, do.trace=TRUE)
fgl.rf <- randomForest(Surprise ~ ., data = TrainingData,mtry = 2, importance = TRUE, do.trace=100)


prediction<-predict(fgl.rf,TestData,type="class")
prediction

library(caret)

fgl.rf3<-train(Surprise~.,data=TrainingData,method="rf",metric='Accuracy',tuneGrid=expand.grid(.mtry=1:6),ntree=500)
fgl.rf3<-train(Surprise~.,data=TrainingData,method="rf",metric='Accuracy',tuneGrid=expand.grid(.mtry=1:6),ntree=500,do.trace=100)
fgl.rf3


#Predicting the values
prediction<-predict(fgl.rf3,TestData,type="raw")
prediction
m.fgl.rf3<-caret::confusionMatrix(prediction,TestData[,10])
m.fgl.rf3

