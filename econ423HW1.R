library(readxl)
library(tidyverse)
library(data.table)
library(AER)

folder<-'C:/Users/luxma/OneDrive/Documents/'
JEC<-read_excel(paste0(folder,'JEC.xls'))
colnames(JEC)

JEC$lnQ<-log(JEC$quantity)
JEC$lnP<-log(JEC$price)
ols<-lm(formula=lnQ~lnP+ice+seas1+seas2+seas3+seas4+seas5+seas6+seas7+seas8+seas9+seas10+seas11+seas12,data=JEC)
summary(ols)
#OLS summary:
ols$coefficients


#IV Regression
reg1<-lm(formula = lnP~cartel,data=JEC)
reg1$coefficients
summary(reg1)
reg2<-ivreg(formula=lnQ~lnP+ice+seas1+seas2+seas3+seas4+seas5+seas6+seas7+seas8+seas9+seas10+seas11+seas12|cartel+ice+seas1+seas2+seas3+seas4+seas5+seas6+seas7+seas8+seas9+seas10+seas11+seas12,data=JEC)
summary(reg2,diagnostics = T)
reg2$coefficients



#Fertility
fertility<-read_excel(paste0(folder,'fertility.xlsx'))
colnames(fertility)
ols<-lm(formula=weeksm1~morekids,data=fertility)
summary(ols)

ols<-lm(formula=morekids~samesex,data=fertility)
summary(ols)

reg2<-ivreg(formula=weeksm1~morekids|samesex,data=fertility)
summary(reg2,diagnostics = T)

reg3<-ivreg(formula=weeksm1~morekids+agem1+black+hispan+othrace|samesex+agem1+black+hispan+othrace,data=fertility)
summary(reg3,diagnostics = T)
