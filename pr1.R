
### Getting and cleaning data
library(caret)
library(rattle)
library(randomForest)
trainfilename <-"train.csv"
trainurl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

testfilename<-"test.csv"
testurl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists(trainfilename))
download.file(trainurl, trainfilename)
training<-read.csv(trainfilename, header = TRUE, na.strings = c("NA", "#DIV/0!"))
if(!file.exists(testfilename))
download.file(testurl, testfilename)
testing<-read.csv(testfilename,header = TRUE, na.strings = c("NA", "#DIV/0!"))

## removing NA
training<-training[,apply(training,2,function (x)!any(is.na(x)))]
testing<-testing[,apply(testing,2,function (x)!any(is.na(x)))]
set.seed(666)
## first 7 collims is info about users - not important in predictions
training<-training[,-c(1:7)]
testing<-testing[,-c(1:7)]

## creating partions on training set

inTrain<-createDataPartition(training$classe, p=0.75, list = FALSE)
test<-training[-inTrain,]
train<-training[inTrain,]

model1<-train(classe~., method="rpart", data = train)
fancyRpartPlot(model1$finalModel)
pred1<-predict(model1, newdata = test)
confusionMatrix(pred1, factor(test$classe)) ##- not very good model

## test2 - random forest
library(randomForest)
train$classe<-as.factor(train$classe)
model2 <- randomForest(classe ~ ., data = train)
pred2<-predict(model2, newdata = test)
confusionMatrix(pred2, as.factor(test$classe))
## test3
predDf<-data.frame(pred1,pred2,classe=test$classe)
combmodfit<-train(classe~.,method = "gam", data = predDf)
combPred<-predict(combmodfit, test$classe)
confusionMatrix(combPred, as.factor(test$classe))

predict(model2,testing)
