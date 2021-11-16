sunspotsts <- ts(data_sunspots$`Monthly Mean Total Sunspot Number` ,start=c(1749, 1), end=c(2021, 1), frequency = 12)

# BoxCox.lambda(sunspotsts)
par(mfrow = c(1,2))
acf(sunspotsts,lag.max = 100)
pacf(sunspotsts,lag.max = 100)
par(mfrow = c(1,1))
# sunspotsts_udiff <- diff(sunspotsts ,differences = 1)

# sunspotsts_udiff

# ggtsdisplay(sunspotsts_udiff)

# pacf(sunspotsts_udiff,lag.max = 100)

#RESIDUALS APPROACH
# SARIMA(0,0,0)x(0,1,0)(12)

FIT1=Arima(sunspotsts, lambda = 0.47, order = c(0,1,0), seasonal = c(0,0,0))
# summary(FIT1)
res1 = rstandard(FIT1)
par(mfrow = c(1,1))
plot(res1)
par(mfrow = c(1,2))
acf(res1, lag.max = 60)
pacf(res1, lag.max = 60)
par(mfrow = c(1,1))
# coeftest(FIT1)

# SARIMA(1,0,0)x(3,1,2)(12)

FIT2=Arima(sunspotsts,lambda = 0.47,order = c(3,1,2), seasonal = c(1,0,0))
res2 = rstandard(FIT2)
coeftest(FIT2)
plot(res2)
par(mfrow = c(1,2))
acf(res2, lag.max = 60)
pacf(res2, lag.max = 60)
par(mfrow = c(1,1))

# coeftest(FIT2)
# ggtsdisplay(FIT2$residuals)

# SARIMA(1,0,2)x(3,1,2)(12)
FIT3=Arima(sunspotsts,lambda = 0.47,order = c(3,1,2), seasonal = c(1,0,2)) #SKIPPED(1,0,1)
res3 = rstandard(FIT3)
coeftest(FIT3)
plot(res3)
par(mfrow = c(1,2))
acf(res3,lag.max = 60)
pacf(res3,lag.max = 60)
par(mfrow = c(1,1))

eacf(res3)

#p,d,q - (0,1,0), (0,1,1), (1,1,1)

# SARIMA(1,0,2)x(0,1,0)(12)
FIT4=Arima(sunspotsts,lambda = 0.47,order = c(0,1,0), seasonal = c(1,0,2))
res4 = rstandard(FIT4)
plot(res4)
par(mfrow = c(1,2))
acf(res4,lag.max = 60)
pacf(res4,lag.max = 60)
par(mfrow = c(1,1))


# SARIMA(1,0,2)x(0,1,1)(12)
FIT5=Arima(sunspotsts,lambda = 0.47,order = c(0,1,1), seasonal = c(1,0,2))
res5 = rstandard(FIT5)
plot(res5)
par(mfrow = c(1,2))
acf(res5,lag.max = 60)
pacf(res5,lag.max = 60)
par(mfrow = c(1,1))


# SARIMA(1,0,2)x(1,1,1)(12)
FIT6=Arima(sunspotsts,lambda = 0.47,order = c(1,1,1), seasonal = c(1,0,2))
res6 = rstandard(FIT6)
plot(res6)
par(mfrow = c(1,2))
acf(res6,lag.max = 60)
pacf(res6,lag.max = 60)
par(mfrow = c(1,1))

#BIC
RES_bic = armasubsets(y=res3 , nar=5 , nma=5, y.name='Average number of sunspots', ar.method='ols')
plot(RES_bic)

# SARIMA(1,0,2)x(3,1,0)(12)

FIT7=Arima(sunspotsts,lambda = 0.47,order = c(3,1,0), seasonal = c(1,0,2))
res7 = rstandard(FIT7)
plot(res7)
par(mfrow = c(1,2))
acf(res7,lag.max = 60)
pacf(res7,lag.max = 60)
par(mfrow = c(1,1))

# RESIDUAL ANALYSIS
par(mar = c(1,1,1,1))

residual.analysis(model = FIT1)
residual.analysis(model = FIT2)
residual.analysis(model = FIT3) #BEST FIT - SARIMA(1,0,2)X(3,1,2)(12)
residual.analysis(model = FIT4)
residual.analysis(model = FIT5)
residual.analysis(model = FIT6)
residual.analysis(model = FIT7)

par(mar = c(1,1,1,1))

# SUMMARY

summary(FIT2)
summary(FIT3)

# HOW WELL THE MODEL FITS

autoplot(sunspotsts) + autolayer(FIT2$fitted)



# FORECAST

Ffrc = forecast(FIT3,h=120)
Ffrc
plot(Ffrc, xlab = "Year", ylab = "Sunspots", main = "Forecasts from ARIMA(3,1,2)(1,0,2)[12]")
