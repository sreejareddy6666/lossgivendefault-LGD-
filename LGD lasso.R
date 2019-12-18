

#import dataset
Loss_Given_Default <-read.csv(file="C:/Users/Admin/Desktop/R/datasets/Loss Given Default.csv")

data<-Loss_Given_Default


## Let's check for any missing values in the data
colSums(is.na(data))

## Checking for empty values
colSums(data=='')

## Check number of uniques values for each of the column 
## to find out columns which we can convert to factors
sapply(data, function(x) length(unique(x)))


###removing unrequired fields

library(dplyr)
Loss_data <- data %>% select(-c( Ac_No))
str(Loss_data)


library(dummies)

Loss_data <- dummy.data.frame(Loss_data, names=c("Gender","Married"))
head(Loss_data)


testindexes <- sample(1:nrow(Loss_data), size=0.2*nrow(Loss_data))
test <- Loss_data[testindexes,]
train <- Loss_data[-testindexes,]


train
summary(train)
#Train the data (fit model)
library(dplyr)
x <- train %>% select(-c(loss, Age, Years.of.Experience))

y<-train$loss
library(lars)
lasso_reg <- lars(as.matrix(x),as.matrix(y),type="lasso")

best_step <- lasso_reg$df[which.min(lasso_reg$RSS)]


#Test the model
regpredicted <- predict(lasso_reg, 
                       as.matrix(test[,1:7]),type = "fit",s=best_step)$fit


#calculate RMSE
library(Metrics)
rmse(actual=test$loss,predicted=regpredicted)

#plot actual vs predicted
regpredicted1<-c(regpredicted)
df<-data.frame(actual=test$loss,predicted=regpredicted1)
par(mfrow=c(1,1))
x<-1:dim(df)[1]; y1=df$actual; y2=df$predicted
plot(x, y1, type="b", pch=19, col="blue", xlab="Index", ylab="Loss",main="Actual Vs Predicted")
# Add a line
lines(x, y2, pch=18, col="red", type="b", lty=2)
# Add a legend
legend("bottomright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty=1:2, cex=0.9)


