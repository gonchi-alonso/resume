library(AER)
data <- read.csv("C:\\Users\\galon\\OneDrive\\Escritorio\\College Stuff\\Year2\\IE330\\Project2\\project2.csv")
state <- data[,1]
mortalityRate <- data[,2]
income <- data[,3]
averageAge <- data[,4]
unemployment <- data[,5]
ratioPH <- as.numeric(data[,6])
population <- as.numeric(data[,7])
uninsured <- data[,8]

model <- lm(mortalityRate~income + averageAge + unemployment + ratioPH +population + uninsured)
summary(model)
resid <- model$residuals

par(mfrow=c(2,2))

qqPlot(resid, main='Normal Probability Plot')

plot(model$fitted.values, resid, main='Y-hat vs Residuals', xlab='Y_hat', ylab='Residual')
abline(h=0, lty=3, col='red')

plot(model$residuals, type='b', main='Observation Order vs Residual', xlab="Observaion Order", ylab="Residual")
abline(h=0, lty=3, col='red')

plot(predict(model),pch=19,col='blue',main='Fitted vs Observed',ylab = 'Observed Values',xlab = 'Fitted Values')
abline(0.07,0,lwd=3,col='red')
