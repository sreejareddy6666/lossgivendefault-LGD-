Loss_Given_Default <-read.csv(file="C:/Users/Admin/Desktop/R/datasets/Loss Given Default.csv")
data<-Loss_Given_Default


# Split data
testindexes <- sample(1:nrow(data), size=0.2*nrow(data))
test <- data[testindexes,]
train <- data[-testindexes,]
summary(train)

#Train the data (fit model)
library(dplyr)
library(randomForest)
set.seed(100)


rf_loss<-randomForest(loss~.,
                      data=train,
                      mtry=3,
                      importance=TRUE)
#Test the model
yhat.rf <- predict(rf_loss,newdata=test)
              
#calculate RMSE
library(Metrics)
rmse(actual=test$loss,predicted=yhat.rf)

#plot actual vs predicted
regpredicted1<-c(regpredicted)
df<-data.frame(actual=test$loss,predicted=yhat.rf)
par(mfrow=c(1,1))
x<-1:dim(df)[1]; y1=df$actual; y2=df$predicted
plot(x, y1, type="b", pch=19, col="blue", xlab="Index", ylab="Loss",main="Actual Vs Predicted")
# Add a line
lines(x, y2, pch=18, col="red", type="b", lty=2)
# Add a legend
legend("bottomright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty=1:2, cex=0.9)


