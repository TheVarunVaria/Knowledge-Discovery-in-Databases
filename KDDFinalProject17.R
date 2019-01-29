install.packages("tm")
library(tm)
setwd("/Users/thevarunvaria/Downloads")
text_corpus<-Corpus(DirSource("500MNTNews"))
writeLines(as.character(text_corpus[[180]]))
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)

stopwords("english")

dtm <- DocumentTermMatrix(text_corpus)
dtm2 <- removeSparseTerms(dtm, sparse=0.95)
dtm3 <-DocumentTermMatrix(text_corpus, control=list(wordLengths=c(4, 20), bounds = list(global = c(5,200))))

dtm3

findFreqTerms(dtm, lowfreq = 200)
findFreqTerms(dtm, lowfreq = 300)
freq <- sort(colSums(as.matrix(dtm3)), decreasing=TRUE)
freq
head(freq,10)
tail(freq,10)

m3<-as.matrix(dtm)
df3<-as.data.frame(m3)
labels<-c(rep("cancer",250),rep("depression",100),rep("diabetes",150))
labels<-as.factor(labels)
df3$label<-labels
str(df3)


# Calculate the entropy of a vector of counts or proportions
# Inputs: Vector of numbers
# Output: Entropy
entropy <- function(p) {
  # Assumes: p is a numeric vector
  if (sum(p) == 0) {
    return(0) 
  }
  p <- p/sum(p) # Normalize so it sums to 1
  p <- p[p > 0] # Discard zero entries (because 0 log 0 = 0)
  H = -sum(p*log(p,base=2))
  return(H)
}
entropy(c(20,5,6,7,9,10))
entropy(c(60,40,100))
entropy(c(250,100,150))



believe_dis<-df3[,"believe"]
food_dis
surprise_dis<-df3[,"surprise"]
human_dis
know_dis<- df3[,"know"]
know_dis
assume_dis<-df3[,"assume"]
assume_dis
think_dis<-df3[,"think"]
think_dis
depression_dis<-df3[,"depression"]
depression_dis
diabetes_dis<-df3[,"diabetes"]
diabetes_dis
cancer_dis<-df3[,"cancer"]
cancer_dis



mydata <- read.csv("ground_truth_one_row_per_article.csv", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))


str(mydata)
summary(mydata)

mydata$Surprise <- (mydata$suprise_r1 + mydata$suprise_r2 + mydata$suprise_r3)/3
mydata$main_category<-NULL

mydata$Believe_dis<- believe_dis
mydata$Surprise_dis<-surprise_dis
mydata$know_dis<-know_dis
mydata$assume_dis<-assume_dis
mydata$think_dis<-think_dis
mydata$depression_dis<-depression_dis
mydata$diabetes_dis<-diabetes_dis
mydata$cancer_dis<-cancer_dis


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


mydata$suprise_r1 <- NULL
mydata$suprise_r2 <- NULL
mydata$suprise_r3 <- NULL
mydata$like_r1 <- NULL
mydata$like_r2 <- NULL
mydata$like_r3 <- NULL



#1,2 - Not Surprised... 4,5 - Surprised
mydata$Surprise <- ifelse(mydata$Surprise <= 3 ,0,1)


#Using following columns as class for our model
mydata$suprise_r1 <- as.factor(mydata$suprise_r1)
mydata$suprise_r2 <- as.factor(mydata$suprise_r2)
mydata$suprise_r3 <- as.factor(mydata$suprise_r3)
mydata$like_r1 <- as.factor(mydata$like_r1)
mydata$like_r2 <- as.factor(mydata$like_r2)
mydata$like_r3 <- as.factor(mydata$like_r3)

mydata$Believe_dis <- as.factor(mydata$Believe_dis)
mydata$Surprise_dis <- as.factor(mydata$Surprise_dis)
mydata$know_dis <- as.factor(mydata$know_dis)
mydata$assume_dis <- as.factor(mydata$assume_dis)

mydata$depression_dis <- as.factor(mydata$depression_dis)
mydata$diabetes_dis <- as.factor(mydata$diabetes_dis)
mydata$cancer_dis <- as.factor(mydata$cancer_dis)





summary(mydata)

#Using Surprise as a factor
mydata$Surprise <- as.factor(mydata$Surprise)


summary(mydata)

#Dividing the data in training data and Test Data
set.seed(10090)
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

###########
fgl.rf2<-caret::confusionMatrix(prediction,TestData[,10])
fgl.rf2
#################

library(caret)

fgl.rf3<-train(Surprise~.,data=TrainingData,method="rf",metric='Accuracy',tuneGrid=expand.grid(.mtry=1:6),ntree=500)
fgl.rf3<-train(Surprise~.,data=TrainingData,method="rf",metric='Accuracy',tuneGrid=expand.grid(.mtry=1:6),ntree=500,do.trace=100)
fgl.rf3


#Predicting the values
prediction<-predict(fgl.rf3,TestData,type="raw")
prediction
m.fgl.rf3<-caret::confusionMatrix(prediction,TestData[,10])
m.fgl.rf3

