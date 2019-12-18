
#Ridge regression code

#import dataset
Loss_Given_Default <-read.csv(file="C:/Users/Admin/Desktop/R/datasets/Loss Given Default.csv")
data<-Loss_Given_Default

# Split data
testindexes = sample(1:nrow(data), size=0.2*nrow(data))
test = data[testindexes,]

train = data[-testindexes,]

#Train the data (fit model)
library(glmnet)
ridge_reg<-glmnet(as.matrix(train[,1:6]),as.matrix(train[,7]),family="gaussian",alpha=0,lambda=0.001)

#Test the model
regpredicted=predict(ridge_reg,as.matrix(test[,1:6]))

#calculate RMSE
library(Metrics)
rmse(actual=test$Loss,predicted=regpredicted)


#plot actual vs predicted
regpredicted1<-c(regpredicted)
df<-data.frame(actual=test$Loss,predicted=regpredicted1)
x<-1:dim(df)[1]; y1=df$actual; y2=df$predicted
plot(x, y1, type="b", pch=19, col="blue", xlab="Index", ylab="Loss",main="Actual Vs Predicted")
# Add a line
lines(x, y2, pch=18, col="red", type="b", lty=2)
# Add a legend
legend("bottomright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty=1:2, cex=0.2)

