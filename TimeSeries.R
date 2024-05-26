library(readxl)
library(tidyverse)
library(data.table)
library(AER)
library(tseries)

folder<-'C:/Users/luxma/OneDrive/Documents/'
df<-read_excel(paste0(folder,'us_macro_quarterly.xlsx'))

gdp<-diff(log(df$GDPC96))

areg<-ar.ols(gdp,order.max = 1,demean=F,intercept=T)
coeftest(areg,vcov. = vcovHC, type = "HC1")
coeftest(areg)


require(dynlm)
ar1<-dynlm(diff(LogGDP) ~ diff(L(LogGDP)))
coeftest(ar1)
confint(ar1)

ar2<-dynlm(diff(LogGDP) ~ diff(L(LogGDP)) + diff(L(LogGDP), 2))
coeftest(ar2)

ar3<-dynlm(diff(LogGDP) ~ diff(L(LogGDP)) + diff(L(LogGDP), 2)+diff(L(LogGDP), 3))
coeftest(ar3)

ar4<-dynlm(diff(LogGDP) ~ diff(L(LogGDP)) + diff(L(LogGDP), 2)+diff(L(LogGDP), 3)+diff(L(LogGDP), 4))
coeftest(ar4)
dynlm(ts(gdp) ~ L(ts(gdp))+L(ts(gdp),2:4))

BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t), 4)
  )
}

AIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "AIC" = log(ssr/t) + npar * 2/t), 4)
  )
}
  
bics<-data.frame(BIC(ar1),BIC(ar2),BIC(ar3),BIC(ar4))
bics
aics<-data.frame(AIC(ar1),AIC(ar2),AIC(ar3),AIC(ar4))
aics


order <- 10:20
AICs <- sapply(order, function(x) 
  "AR" = AIC(dynlm(ts(gdp) ~ L(ts(gdp), 1:x))))


#ADF
adf.test(gdp,alternative='stationary',k=3)
aug_ar3<-dynlm(ts(gdp) ~ L(ts(df$GDPC96), 1)+L(ts(gdp),1)+L(ts(gdp),2)+ts(trend(log(df$GDPC96),scale=F)))
coeftest(aug_ar3)

urca::ur.df(gdp,lags=3,type='trend')

LogGDP<-ts(log(df$GDPC96))
coeftest(
  dynlm(diff(LogGDP) ~ trend(LogGDP, scale = F) + L(LogGDP) 
        + diff(L(LogGDP)) + diff(L(LogGDP), 2)))
