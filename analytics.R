# DSC 425 PROJECT
# 12/01/19
# CHRIS SCHAFFER

library(moments)
library(ggplot2)
library(tseries)
library(astsa)
library(forecast)
library(fpp2)
library(dplyr)
library(PerformanceAnalytics)

# ==============================================
# BRAZILLIAN FOREST FIRES
# ==============================================

t = 0.75

# LOAD DATAFRAME
df = read.csv(file = "C:/Users/Not Your Computer/Dropbox/Grad School/DSC 425/amazon_allfires.csv", header = TRUE, sep = ",")
fires = df$totals[6:length(df$totals)]
fires = ts(fires, frequency = 12, start = c(1998, 6), end = c(2017, 11))
par(mfrow = c(1,2))
acf(fires, main = 'Autocorrelation', xlab = 'Lag (years)')
pacf(fires, main = 'Partial Autocorrelation', xlab = 'Lag (years)')

# PLOT ALL DATA
par(mfrow = c(1,1))
ts.plot(fires, main = '20-Year Forest Fire Totals by Month', xlab = 'Time', ylab = 'Monthly Fire Total')
hist(fires, breaks = 50, xlab = 'Monthly Totals', ylab = 'Count', main = 'Histogram of Monthly Fire Totals', c = 'orange')
boxplot(fires, horizontal = T, main = 'Boxplot of Nationwide Monthly Totals', xlab = 'Monthly Totals')

# TESTS
jarque.bera.test(fires)
Box.test(fires, type = "Ljung-Box", fitdf = 0)
kpss.test(fires)
adf.test(fires)

train = fires[1:177]
train = ts(train, start = c(1998,6), frequency = 12)
test = fires[176:length(fires)]
test = ts(train, start = c(2013,1), end = c(2017,11), frequency = 12)

# ==============================================
# REGRESSION MODELS
# ==============================================
M1 = tslm(train~trend+season, data = train)
#M1 = arima(train, order = c(13,0,12))

print(M1)
C = M1$coefficients
par(mfrow = c(1,1))
barplot(C, main = 'Regression Model Coefficients', ylab = 'Weight', xlab = 'Model Coefficient', col = 'lightblue')
summary(M1)

# ==============================================
# RESIDUAL ANALYSIS
# ==============================================
e1 = resid(M1)
#e = e[is.na(e) == F]
mean(e1)
sd(e1)
print(c(mean(e1)-1.96*sd(e1),mean(e1)+1.96*sd(e1)))
jarque.bera.test(e1)
Box.test(e1, lag = 1, type = "Ljung-Box", fitdf = 0)
kpss.test(e1)
adf.test(e1)

# PLOT RESIDUALS
par(mfrow = c(1,1))
boxplot(e1, horizontal = T, main = 'Box Plot of Residuals', xlab = 'Error (No. Fires)')
par(mfrow = c(1,2))
hist(e1, breaks = 50, c = 'lightblue', main = 'Histogram of Residuals', xlab = 'Error', ylab = 'Count')
qqnorm(e1, main = 'Normal Q-Q Plot of Residuals')
qqline(e1)
par(mfrow = c(1,2))
acf(e1, main = 'ACF of Residuals', ylab = 'Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
pacf(e1, main = 'PACF of Residuals', ylab = 'Partial Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
rmse1 = sqrt(sum(e1**2/length(e1)))
mape1 = sum(abs(e1/train))/length(e1)*100
mae = mean(abs(e1))
checkresiduals(M1)

# ==============================================
# FORECASTING
# ==============================================
par(mfrow = c(1,1))
n = length(test)
p1 = forecast(M1, h = n)

plot(p1, main = '5-Year Monthly Fire Forecast', ylab = 'Monthly Fires')
lines(fires, col = 'red')

plot(p1, main = '5-Year Monthly Fire Forecast', ylab = 'Monthly Fires', include = 24)
lines(fires, col = 'red')

# ==============================================
# FORECAST RESIDUALS
# ==============================================
ef = p1$mean - test
mean(ef)
sd(ef)
print(c(mean(ef)-1.96*sd(ef),mean(ef)+1.96*sd(ef)))
jarque.bera.test(ef)
Box.test(ef, lag = 1, type = "Ljung-Box", fitdf = 0)
kpss.test(ef)
adf.test(ef)

# PLOT RESIDUALS
par(mfrow = c(1,1))
boxplot(ef, horizontal = T, main = 'Box Plot of Forecast Residuals', xlab = 'Error (No. Fires)')
par(mfrow = c(1,2))
hist(ef, breaks = 50, c = 'lightblue', main = 'Histogram of Residuals', xlab = 'Error', ylab = 'Count')
qqnorm(ef, main = 'Normal Q-Q Plot of Residuals')
qqline(ef)
par(mfrow = c(1,2))
acf(ef, main = 'ACF of Residuals', ylab = 'Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
pacf(ef, main = 'PACF of Residuals', ylab = 'Partial Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
rmsef = sqrt(sum(ef**2/length(ef)))
mapef = sum(abs(ef/test))/length(ef)*100
maef = mean(abs(ef))

# # ==============================================
# # DIFFERENCED VALUES
# # ==============================================
# d = diff(fires, lag = 1)
# 
# jarque.bera.test(d)
# Box.test(d, type = "Ljung-Box", fitdf = 0)
# kpss.test(d)
# adf.test(d)
# 
# par(mfrow = c(1,2))
# acf(d, main = 'ACF of Differences', ylab = 'Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
# pacf(d, main = 'PACF of Differences', ylab = 'Partial Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
# 
# par(mfrow = c(1,1))
# ts.plot(d, main = 'Differences in Nationwide Forest Fire Counts', xlab = 'Time', ylab = 'Difference')
# abline(h = mean(d)+2*sd(d), col = 'red', lty = 2, lwd = 2)
# abline(h = mean(d)-2*sd(d), col = 'red', lty = 2, lwd = 2)
# abline(h = mean(d)+3*sd(d), col = 'orange', lty = 2, lwd = 2)
# abline(h = mean(d)-3*sd(d), col = 'orange', lty = 2, lwd = 2)
# 
# par(mfrow = c(1,2))
# hist(d, breaks = 50, xlab = 'Monthly Difference', ylab = 'Count', main = 'Histogram of Differences', c = 'orange')
# qqnorm(d, main = 'Normal Q-Q Plot of Differences')
# qqline(d)
# 
# train_d = ts(d[0:round(t*length(d))])
# test_d = ts(d[round(t*length(d)):length(d)])
# 
# 
# # ==============================================
# # REGRESSION MODELS
# # ==============================================
# M2 = arima(train_d, order = c(12,0,12))
# train_d = ts(train_d, start = c(1998,6), end = c(2017,11), frequency = 12)
# M2 = tslm(train_d~trend+season, data = train_d)
# print(M2)
# C = M2$coef
# par(mfrow = c(1,1))
# barplot(C, main = 'Regression Model Coefficients', ylab = 'Weight', xlab = 'Model Coefficient', col = 'lightblue')
# 
# # ==============================================
# # RESIDUAL ANALYSIS
# # ==============================================
# e2 = resid(M2)
# #e = e[is.na(e) == F]
# mean(e2)
# sd(e2)
# quantile(e2)
# print(c(mean(e2)-1.96*sd(e2),mean(e2)+1.96*sd(e2)))
# jarque.bera.test(e2)
# Box.test(e2, lag = 1, type = "Ljung-Box", fitdf = 0)
# 
# # PLOT RESIDUALS
# par(mfrow = c(1,1))
# boxplot(e2, horizontal = T, main = 'Box Plot of Residuals', xlab = 'Error (No. Fires)')
# par(mfrow = c(1,2))
# hist(e2, breaks = 50, c = 'lightblue', main = 'Histogram of Residuals', xlab = 'Error', ylab = 'Count')
# qqnorm(e2, main = 'Normal Q-Q Plot of Residuals')
# qqline(e2)
# par(mfrow = c(1,2))
# acf(e2, main = 'ACF of Residuals', ylab = 'Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
# pacf(e2, main = 'PACF of Residuals', ylab = 'Partial Autocorrelation', xlab = 'Lag (years)', lag.max = 48)
# rmse2 = sqrt(sum(e2**2/length(e2)))
# mape2 = sum(abs(e2/train))/length(e2)*100
# checkresiduals(M2)
# 
# p = numeric(length(test))
# p[1] = tail(train, n=1) + p2$mean[1]
# for (i in 2:length(test)){
#   change = p2$mean[i]
#   p[i] = p[i-1] + change
# }
# p = ts(p, st = c(2013,1), end = c(2017,11), frequency = 12)
# plot(test)
# lines(p, col = 'red')
# 
# # ==============================================
# # FORECASTING
# # ==============================================
# par(mfrow = c(1,1))
# n = length(test)
# p2 = forecast(M2, h = n)
# 
# plot(p2, main = 'Forecasted Fires', ylab = 'Monthly Fires')
# lines(fires, col = 'red')
# 
# plot(p2, main = 'Forecasted Fires', ylab = 'Monthly Fires', include = 24)
# lines(fires, col = 'red')
# 
# 
# plot(p2$mean, main = 'Forecasted Fires', ylab = 'Monthly Fires', col = 'blue')
# lines(fires, col = 'red')
# lines(p1$mean, col = 'green')
# 
# plot(forecast(M1, h = n),include = 24)
# lines(fires, col = 'red')
# plot(forecast(M2, h = n),include = 24)
# lines(fires, col = 'red')

# MODEL BY STATE
# df2 = read.csv(file = "C:/Users/Not Your Computer/Dropbox/Grad School/DSC 425/amazon_clean.csv", header = TRUE, sep = ",")
# n = 36
# p_total = numeric(length(test))
# for (st in unique(df2$state)){
#   sdf = df2[df2$state == st, 1:5]
#   #ts.plot(ts(sdf$number), main = st, ylab = 'Monthly Fire Totals')
#   state_d = ts(diff(sdf$number,lag=1))
#   state_d = state_d[120:length(state_d)]
#   par(mfrow = c(1,2))
#   acf(state_d)
#   pacf(state_d)
#   print(st)
#   print(Box.test(state_d, lag = 1, type = "Ljung-Box", fitdf = 0)$p.value)
#   par(mfrow = c(1,1))
#   #ts.plot(state_d, main = st, ylab = 'Monthly Fire Totals')
#   train = state_d[1:211]
#   test = state_d[212:238]
#   M = arima(train, order = c(12,0,12), optim.control = list(maxit = 2000))
#   p = predict(M, n.ahead = n)$pred[1:n]
#   par(mfrow = c(1,1))
#   plot(forecast(M, h = n, level = c(68,95)), main = '5-Year Monthly Forecasted Differences', xlab = 'Month Index', ylab = 'Monthly Change in Fire Count', include = n)
#   lines(as.numeric(state_d), col = 'red', lwd=2)
#   p_total = p_total + p
# }
# 
