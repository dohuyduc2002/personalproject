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
#Import data
chung <- read_excel("chung.xlsx", sheet = "Sheet2", 
                    col_types = c("numeric", "numeric"))

CMG1 <- chung$CMG
CTR1 <- chung$CTR
CMG1 <- ts(CMG1)
CTR1 <- ts(CTR1)
a <- data.frame(CTR1,CMG1)
a <- ts(a)
autoplot(a) + labs(title = "CMG & CTR") + ylab("Price")
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

save(chung,file = "TOKT_G20.rda")

