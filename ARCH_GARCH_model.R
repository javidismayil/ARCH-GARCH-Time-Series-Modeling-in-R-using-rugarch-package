library(dplyr)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
df<-getSymbols("TSLA", from="2010-01-01", to="2020-12-31")
chartSeries(TSLA )
head(df)
chartSeries(TSLA["2020-12"] )
returnn<-CalculateReturns(TSLA$TSLA.Adjusted)
returnn<-returnn[-c(1),]
chart_Series(returnn)
chart.Histogram(returnn, methods = c("add.density", "add.normal"), 
                colorset = c("blue", "red", "black"))
legend("topright", legend = c("return", "kernel", "normal dist"), fill = c("blue", "red", "black"))
sd(returnn)
sqrt(252)*sd(returnn["2020"])
chart.RollingPerformance(R=returnn["2010::2020"], with=22, FUN = "sd.annualized", scale=252, 
                         main ="TSLA`s monthly votality" )
mod_specify<-ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                        variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
                        distribution.model = "norm")
mod_fitting<-ugarchfit(data=returnn, spec = mod_specify, out.sample = 20)
mod_fitting
plot(mod_fitting, which='all')


garchm_specify<-ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE), 
                        variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
                        distribution.model = "norm")
garchm_fitting<-ugarchfit(data=returnn, spec = garchm_specify, out.sample = 20)
plot(garchm_fitting)


garcht_specify<-ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                           variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)), 
                           distribution.model = "norm")
garcht_fitting<-ugarchfit(data=returnn, spec = garcht_specify, out.sample = 20)
plot(garchm_fitting)

garche_specify<-ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                           variance.model = list(model="eGARCH", garchOrder=c(1,1)), 
                           distribution.model = "norm")
garche_fitting<-ugarchfit(data=returnn, spec = garche_specify, out.sample = 20)
plot(garche_fitting)

garchgjr_specify<-ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                           variance.model = list(model="gjrGARCH", garchOrder=c(1,1)), 
                           distribution.model = "norm")
garchgjr_fitting<-ugarchfit(data=returnn, spec = garchgjr_specify, out.sample = 20)
plot(garche_fitting)

garchgjr_fitting


start_date <- as.Date("2021-01-01")
end_date <- Sys.Date()
aa<-getSymbols("ETH-USD", src = "yahoo", from = start_date, to = end_date)
aa<-`ETH-USD`
chartSeries(aa)
head(aa)
bands<-TTR::BBands(aa[,2:4])
chartSeries(aa[,2:4], TA = "addBBands(n=20, sd=2)")


