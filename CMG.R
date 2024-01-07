library(readxl)
library(dplyr)
library(ggplot2)
library(tsutils)
library(lmtest)
library(Metrics)
library(car)
library(imputeTS)
library(forecast)
library(tseries)
library(urca)
library(astsa)
library(dslabs)
library(corrplot)
library(stats)
library(PerformanceAnalytics)
dat <- ts(CMG,start = c(2009,3),frequency = 4)
class(dat)
plot.ts(dat)
fre <- ts(CMG$Data,start = c(2009,3),frequency = 4)
plot(fre)

#ts.cumsum use to inverse seasonal variable in reg
#model1 : trend,seasonal,add, lin-lin
dumm <- seasdummy(51,4)
trend <- seq_along(fre)
t1 <- c(0,1,rep(c(0,1,0,0),12),0,1,0)
t2 <- c(1,0,rep(c(0,0,1,0),12),0,0,1)
t3 <- c(0,1,rep(c(0,0,0,1),12),0,0,0)
print(t1)

model1 <- lm(dat ~ trend + t1 + t2 + t3)
summary(model1)
fit1 <- fitted(model1)
a<- decompose(dat)
plot(a)
bgtest(model1)
bptest(model1)
ac1 <- pull(CMG,2)
ac1 <- na.omit(ac1)
mape(ac1,fit1)
rmse(ac1,fit1)
p1 <- data.frame(trend = 51:58,t1 = c(0,1,0,0,0,1,0,0),t2=c(0,0,1,0,0,0,1,0),t3 =c(0,0,0,1,0,0,0,1))
pre1 <- predict(model1,p1)
pre1
p1
ac1[50:52]
mape(ac1[50:52],pre1[1:3,2])
#Model overfit -> need more data, no autocorr, var is equal
ac1[50:52]
pre1[1:3,2]
#model2: trend,seas,multi
model2 <- lm(dat ~ trend + trend*t1 +trend*t2+trend*t3)
summary(model2)
fit2 <- fitted(model2)
mape(ac1,fit2)
pre2
pre2 <- predict(model2,p1)
mape(ac1[50:52],pre2[1:3,2])
fit2
#model3
frea <- na_kalman(fre)
print(frea)
frea <- frea[1:50]
frea <- ts(frea,start = c(2009,3),frequency = 4)
model3 <- hw(frea,seasonal = "additive")
model3
summary(model3)
fit3
fitted(model3)
fit3 <- fitted(model3)[1:50]
pre3 <- predict(model3,8)
mape(fitted(model3)[1:50],ac1[1:50])
mape(pre3[1:3],ac1[50:52])

#model4
model4 <- hw(frea,seasonal = "multiplicative")

model4 <- HoltWinters(frea,seasonal = "multiplicative")
model4
fit4 <- fitted(model4)[1:50]
pre4 <- predict(model4,8)

forecast(model4,8)
mape(fitted(model4)[1:50],ac1[1:50])
mape(pre4[1:3],ac1[50:52])

daily <- pull(CMG,-2)
plot(daily, type = "l")


acf(daily)
pacf(daily)
daily <- ts(daily)
adf.test(daily)
# non stationary 

log_daily <- log(daily,30)
pacf(log_daily)
acf(log_daily)
adf.test(log_daily)

sqrt_daily <- sqrt(daily)
adf.test(sqrt_daily)

dif1 <- diff(daily, lag = 1)

adf.test(dif1)
acf(dif1)
pacf(dif1)
plot(dif1, type = "l")
#Stationary, AR2,MA2
Arima(dif1,order = c(13,1,13))
arima(dif1,order = c(2,1,2))      
auto.arima(dif1)

summary(ur.df(dif1,type = "none",lags = 1))
summary(ur.df(dif1,type = "drift",lags = 1))
summary(ur.df(dif1,type = "trend",selectlags = "AIC"))
#lag = 1 

arima13 <- Arima(dif1,order = c(2,1,2))
checkresiduals(arima13)
arima13
auto.arima(dif1)
prear13 <- predict(arima13)
fitpre <- predict(arima13)$pred
fitse <- predict(arima13)$se
points(fitpre - 2 * fitse,type = "l", col = 2, lty = 2)

auto.arima(dif1)
# accuracy function to test forcast value (MAPE,MSE,MAE,...) between train and test set 
#par to plot all plot in 1 window
sarima.for(daily,n.ahead = 11,2,1,2,0,0,0,0)

real <- c(41.500,41.100,41.200,40.950,40.000,41.400,40.850,40.500,40.250,40.300,40.500)


arima1 <-Arima(daily,order = c(2,1,2))
checkresiduals(arima1)

a <- predict(arima1,11)
arimapre <- a$pred
mape(arimapre,real)
rmse(arimapre,real)
accuracy(arimapre,real)
real
arimapre

arimapre1 <- arima1$fitted
real1 <- CMG$`Daily-price`
accuracy(arimapre1,real1)
arima1



#Sporious reg
dif_CMG <- diff(CMG1,1)
dif_CTR <- diff(CTR1,1)


t1 <- autoplot(dif_CTR, color = "#26969E",main = "CTR difference", ylab =" Difference")
t2<- autoplot(dif_CMG, color = "#9E5726",main = "CMG difference", ylab =" Difference")
grid.arrange(t1,t2,ncol = 2)
summary(ur.df(dif_CMG))
summary(ur.df(dif_CTR))

summary(lm(CMG1 ~ CTR1))
summary(lm(CTR1 ~CMG1))
summary(ur.df(resid(lm(CMG1 ~ CTR1))))
summary(ur.df(resid(lm(CTR1 ~ CMG1))))

residCMG <- resid(lm(CMG1 ~ CTR1))
residCTR <- resid(lm(CTR1 ~ CMG1))
#cointergrated 
CMG1 <- chung$CMG
CTR1 <- chung$CTR
CMG1 <- ts(CMG1)
CTR1 <- ts(CTR1)
a <- data.frame(CTR1,CMG1)
a <- ts(a)
autoplot(a) + labs(title = "CMG & CTR") + ylab("Price")

dif_CMG <- diff(CMG1,1)
dif_CTR <- diff(CTR1,1)

data_dif <- data.frame(dif_CMG,dif_CTR)
data_dif <- cbind(data_dif,index(data_dif))
colnames(data_dif) <- c("dif_CMG","dif_CTR","Time")

data_dif1 <- data.frame(dif_CMG,dif_CTR)


data1 <- data.frame(CMG1,CTR1)
coin1 <- ca.jo(data1,type = "trace")
summary(coin1)
coin2 <- ca.jo(data1,type ="eigen")
summary(coin2)
summary(lm(CMG1 ~ CTR1))
summary(lm(CTR1 ~ CMG1))
# cointergrated 

#ECM
summary(lm(dif_CMG ~ dif_CTR + residCMG[1:498]))


#VAR
grangertest(dif_CMG,dif_CTR,order = 1)
grangertest(dif_CTR,dif_CMG,order = 1)
#Not reject both null hypothesis
data_dif1 <- as.ts(data_dif1)
VARselect(data_dif1,lag.max = 5 )
var1 <- VAR(data_dif1,p=1,type = "trend")


summary(var1)
serial.test(var1)
plot(predict(var1,n.ahead = 100))
pre_var <- predict(var1)
pre_var$fcst$dif_CMG

a <- resid(var1)
data1
preCMG <- pre_var$fcst$dif_CMG[,1]
preCTR <- pre_var$fcst$dif_CTR[,1]

fc.CMG <- c(rep(41.5,11))
fc.CTR <- c(rep(53,11))
for(i in 2:11){
  fc.CMG[i] = fc.CMG[i-1] + preCMG[i-1]
}

for(i in 2:11){
  fc.CTR[i] = fc.CTR[i-1] + preCTR[i-1]
}
b <- ts(data1)
fc.CMG <- ts(fc.CMG,start = 500)
fc.CTR <- ts(fc.CTR,start = 500)
b <- cbind(fc.CMG,fc.CTR)
realCTR <- c(53,52.9,53.2,52.5,52.8,52.5,53.6,53.7,53.7,53.7)
realCMG <- c(41.500,41.100,41.200,40.950,40.000,41.400,40.850,40.500,40.250,40.300)
realCMG <- ts(realCMG,start = 500)
realCTR <- ts(realCTR,start = 500)
c <- cbind(realCMG,realCTR)
plotvar1 <- autoplot(realCMG, series = "Real CMG") + autolayer(fc.CMG,series = "Forcast CMG") + xlab("CMG") + ggtitle("Forcast CMG using VAR")
plotvar2 <- autoplot(realCTR, series = "Real CTR") + autolayer(fc.CTR,series = "Forcast CTR") + xlab("CTR") + ggtitle("Forcast CTR using VAR")

plotvar1 + plotvar2

irf(var1)
par(mfrow = c(1,2))
plot(irf(var1))

fevd(var1, n.ahead = 10)
plot(fevd(var1, n.ahead = 10))

