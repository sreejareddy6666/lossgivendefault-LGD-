
#import dataset
Loss_Given_Default <-read.csv(file="C:/Users/Admin/Desktop/R/datasets/Loss Given Default.csv")
data<-Loss_Given_Default

#split data
testindexes = sample(1:nrow(data), size=0.2*nrow(data))
test = data[testindexes,]
train = data[-testindexes,]
set.seed(100)

library(rpart)
library(rpart.plot)



tree <- rpart(loss~.,data=train)
rpart.plot(tree)

predictions <- predict(tree,test)
library(Metrics)
rmse(test$loss,predictions)

