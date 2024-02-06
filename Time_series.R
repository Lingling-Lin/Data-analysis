# Author: Lingling Lin


library(tikzDevice)
library(latex2exp)
library(astsa)

data <- read.csv("IPG2211A2N.csv")
colnames(data) <- c("Date", "IPIndex")
x <- ts(data$IPIndex, start = c(1985,1,1), end = c(2021,1,1), frequency = 12)

## Plot the time series
ts.plot(x, ylab = 'Index 2012 = 100', main = 'Industrial Production of Electric and Gas Utilities')
lines(ksmooth(time(x), x, "normal", bandwidth=2), lwd=2, col=4)

## Take transformation to make stationary time series
llx <- log(x)
tstrans <- diff(llx)
#plot(tstrans, ylab = '', main = TeX('$tstrans(\\nabla\\log x_t)$'))
acf(tstrans, main = TeX('Series tstrans$(\\nabla\\log x_t)$'))
dts = diff(tstrans,12)
plot(dts, ylab = '', main = TeX('$dts(\\nabla_{12}\\nabla\\log x_t)$'))
abline(h = mean(dts), col = 'red')
acf2(dts)
adf.test(dts)

## Possible ARIMA models with diagnostics
sarima(log(x), 1,1,1, 0,1,2, 12)
sarima(log(x), 1,1,1, 0,1,3, 12)
sarima(log(x), 1,1,1, 2,1,2, 12) ## the winner
#sarima(log(x), 1,1,1, 2,1,1, 12)

## Forecast next 10 months with prediction interval
fitt = sarima.for(log(x),11, 1,1,1, 2,1,2, 12)
predict1 = exp(fitt$pred)
U = exp(fitt$pred + fitt$se)
L = exp(fitt$pred - fitt$se)
UU = exp(fitt$pred + 2*fitt$se)
LL = exp(fitt$pred - 2*fitt$se)

ts.plot(x, predict1, col = 1:2, xlim = c(2010,2022), main = "Forecasting IPEG for the next 11 months", ylab = expression(x))
grid(NULL, NULL, lty = 1, col = "gray88")
xx = c(time(U), rev(time(U))) ; yy = c(L, rev(U))
xxx = c(time(UU), rev(time(UU))); yyy = c(LL, rev(UU))
polygon(xx,yy, border = 8, col = gray(.6,alpha = .3))
polygon(xxx,yyy, border = 8, col = gray(.6,alpha = .2))
lines(predict1, type="p", col=2)
lines(x, type = 'p', col = 1)


