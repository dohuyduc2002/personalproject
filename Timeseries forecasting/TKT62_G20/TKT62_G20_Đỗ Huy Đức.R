load(file = 'CMG.rda')
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
library(PerformanceAnalytics)
library(graphics)
library(stats)
library(patchwork)
#I.Phân tích chỉ số lợi nhuận gộp của CMG
#Import,cleaning data
CMG_base <- read_excel("CMG.xlsx", sheet = "CMG", 
                  col_types = c("text", "numeric"))
CMG <- na_kalman(CMG_base)
dat <- ts(CMG,start = c(2009,3),frequency = 4)
dat_train <- ts(dat[1:50,2],start =c(2009,3),frequency = 4)
dat_test <- ts(dat[51:53,2],start = c(2022,1),frequency = 4)
class(dat_train)
plot.ts(dat_train)

trend <- seq_along(dat_train)
t2 <- c(0,0,rep(c(0,1,0,0),12))
t3 <- c(1,0,rep(c(0,0,1,0),12))
t4 <- c(0,1,rep(c(0,0,0,1),12))
p1 <- data.frame(trend = 51:58,t2 = c(0,1,0,0,0,1,0,0),t3=c(0,0,1,0,0,0,1,0),t4 =c(0,0,0,1,0,0,0,1))

#Decompose
de_train <- decompose(dat_train)
plot(de_train)
de1<- autoplot(de_train$seasonal,main = "Seasonal component", ylab = "Data")
de2<- autoplot(de_train$trend,main = "Trend component", ylab = "Data")

#Tạo biến seasonal tương tứng với t2 là quý 2, tương tự với t3,4
#1.Model 1 : Trend, linear-linear 
model1 <- lm(dat_train ~ trend)
summary(model1)
fit1 <- fitted(model1)
pre1 <- predict(model1,p1)
pre1 <- ts(pre1,start = c(2022,1),frequency = 4)
mape(dat_test,pre1)
mape(dat_train,fit1)

#2.Model 2: Trend, linear-log 
model2 <- lm(dat_train  ~ log(trend))
summary(model2)
fit2 <- fitted(model2)
pre2 <- predict(model2,p1)
pre2 <- ts(pre2,start = c(2022,1),frequency = 4)
mape(dat_test,pre2)
mape(dat_train,fit2)

#5.Model 3 : Trend-seas, add, 
model3 <- lm(dat_train  ~ trend + t2 + t3 + t4)
summary(model3)
fit3 <- fitted(model3)
pre3 <- predict(model3,p1)
pre3 <- ts(pre3,start = c(2022,1),frequency = 4)
mape(dat_test,pre3)
mape(dat_train,fit3)

#Model 4 : trend-seas,multi
model4 <- lm(dat_train  ~ trend + trend*t2 +trend*t3+trend*t4)
summary(model4)
fit4 <- fitted(model4)
pre4 <- predict(model4,p1)
pre4 <- ts(pre4,start = c(2022,1),frequency = 4)
mape(dat_test,pre4)
mape(dat_train,fit4)
#model 5 : holt-winter add
model5 <- HoltWinters(dat_train,seasonal ="additive")
model5
pre5<- predict(model5,8)
mape(dat_test,pre5)
mape(dat_train,model5$fitted)


#model 6: holt_winter multi
model6 <- HoltWinters(dat_train,seasonal = "multiplicative")
pre6 <- predict(model6,8)
mape(dat_test,pre6)
mape(dat_train,model6$fitted)
#Vẽ đồ thị dự báo 
#Holt-winter
plot1 = autoplot(dat_test)+
  autolayer(pre5, series="HW additive forecasts") +
  autolayer(pre6, series="HW multiplicative forecasts") +
  xlab("Year")+
  ylab("Post-tax income")+
  ggtitle("CMG")+
  guides(colour=guide_legend(title="Forecast"))
#Trend-seasonal  
plot2 = autoplot(dat_test)+
  autolayer(pre3, series="trend-sesonal additive forecasts") +
  autolayer(pre4, series="trend-sesonal multiplicative forecasts") +
  xlab("Year")+
  ylab("Post-tax income")+
  ggtitle("CMG")+
  guides(colour=guide_legend(title="Forecast"))
#Trend
plot3 = autoplot(dat_test)+
  autolayer(pre1, series="trend lin-lin forecasts") +
  autolayer(pre2, series="trend lin-log forecasts") +
  xlab("Year")+
  ylab("Post-tax income")+
  ggtitle("CMG")+
  guides(colour=guide_legend(title="Forecast"))

#II.Phân tích giá đóng cửa của CMG
#Import data
daily_CMG <- read_excel("CMG.xlsx", sheet = "Sheet1", 
                        col_types = c("numeric", "numeric"))
price <- pull(daily_CMG,-2)
ggplot(daily_CMG,aes(x = index(price), y = price)) + geom_line( size = 0.7,color = "#956868") + theme_linedraw() +ggtitle("CMG daily close price") + xlab("Daily")

#Infrerential statistics
inf_stat <- summary(price)
sd(price)
ggplot(daily_CMG,aes(y = price, x = index(price))) + geom_boxplot() + xlab("Index") + ylab("Price") + theme_gray() + ggtitle("Boxplot CMG daily close price")
jarque.bera.test(price)
pchisq(0.95,2)


#ARIMA(3,1,2)
par(mfrow=c(1,2))
acf(price)
pacf(price)
df_CMG <- summary(ur.df(price,type = "none",selectlags = "AIC"))
price_dif <- diff(price,1)
acf(price_dif)
pacf(price_dif)

summary(ur.df(price_dif,type = "none",selectlags = "AIC"))
summary(ur.df(price_dif,type = "drift",selectlags = "AIC"))
df_deltaCMG <- summary(ur.df(price_dif,type = "trend",selectlags = "AIC"))
#Trend is significant
# => Random walk, need to stationary


#From acf,pacf, of difference series, we choose ARIMA(3,1,2)
model <- Arima(price,order = c(3,1,2),include.drift = TRUE)
checkresiduals(model)
pre_arma <- forecast(model, h =11)
plot(pre_arma, xlab = "Time",ylab = "Price")
pre_arma
autoplot(model)
real <- c(41.500,41.100,41.200,40.950,40.000,41.400,40.850,40.500,40.250,40.300,40.500,41)
pre <- c(39.89696,39.79403,39.74976,39.74624,39.74101,39.74632,39.74337,39.74809,39.74566,39.74980, 39.74786,39.75153)

real <- ts(real, start = 500, frequency = 1)
autoplot(ts(pre_arma$mean, start = 500, frequency = 1),series = "Forcasted Price") + autolayer(real, series = "actual prices") + ylab("Price") + ggtitle("Forcasting CMG using ARIMA(3,1,2)")


#ARIMA(3,1,13)
model_arima <- Arima(price,order = c(13,1,13),include.drift = TRUE)
checkresiduals(model_arima)
autoplot(model_arima)
pre_arma1 <- forecast(model_arima, h =11)
real <- ts(real, start = 500, frequency = 1)
autoplot(ts(pre_arma1$mean, start = 500, frequency = 1),series = "Forcasted Price") + autolayer(real, series = "actual prices") + ylab("Price") + ggtitle("Forcasting CMG using ARIMA(13,1,13)")

save(CMG_base,daily_CMG,file = "CMG")


