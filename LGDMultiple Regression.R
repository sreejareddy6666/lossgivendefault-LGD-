#import dataset
Loss_Given_Default <-read.csv(file="C:/Users/Admin/Desktop/R/datasets/Loss_Given_Default.csv")
data<-Loss_Given_Default

# Split data
testindexes = sample(1:nrow(data), size=0.2*nrow(data))
test = data[testindexes,]
train = data[-testindexes,]

#Train the data (fit model)
regmodel=lm(loss~.,data=train)
par(mfrow=c(2,2))
plot(regmodel)
summary(regmodel)

#Test the model
regpredicted=predict(regmodel,test)

#calculate RMSE
library(Metrics)
rmse(actual=test$loss,predicted=regpredicted)

#plot actual vs predicted
df<-data.frame(actual=test$loss,predicted=regpredicted)
x<-1:dim(df)[1]; y1=df$actual; y2=df$predicted
plot(x, y1, type="b", pch=19, col="blue", xlab="Index", ylab="Losses.in.Thousands",main="Actual Vs Predicted")
# Add a line
lines(x, y2, pch=18, col="red", type="b", lty=2)

# Add a legend
legend("bottomright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty=1:2, cex=0.2)

