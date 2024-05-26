library(readxl)
library(tidyverse)
library(data.table)
library(AER)

folder<-'C:/Users/luxma/OneDrive/Documents/'
df<-read_excel(paste0(folder,'WeakInstrument.xlsx'))
colnames(df)

reg1<-lm(formula = x~z,data=df)
summary(reg1)
reg2<-ivreg(formula=y~x|z,data=df)
summary(reg2,diagnostics = T)
confint(reg2, 'x', level=0.95)

coeftest(AER::ivreg(y ~ x | z , data=df),type="HC0",vcov. = sandwich::vcovCL)
