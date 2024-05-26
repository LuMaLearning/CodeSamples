library(readxl)
library(tidyverse)
library(data.table)
library(AER)
library(tseries)
library(plm)

folder<-'C:/Users/luxma/OneDrive/Documents/'
df<-read_excel(paste0(folder,'Guns.xlsx'))
df$lnvio<-log(df$vio)

reg1<-lm(formula=log(vio)~shall,data=df)
summary(reg1)

reg2<-lm(formula=log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029,data=df)
summary(reg2)

reg3<-plm(formula=log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029,data=df,
          index=c('stateid','year'),
          model='within')
summary(reg3)

reg4<-plm(formula=log(vio)~shall+incarc_rate+density+avginc+pop+pb1064+pw1064+pm1029,data=df,
          index=c('stateid','year'),
          model='within',
          effect='time')
summary(reg4)


#Traffic
df<-read_excel(paste0(folder,'SeatBelts.xls'))
colnames(df)
reg1<-lm(formula=fatalityrate~sb_useage+speed65+speed70+ba08+drinkage21+log(income)+age,data=df)
summary(reg1)

reg2<-plm(formula=fatalityrate~sb_useage+speed65+speed70+ba08+drinkage21+log(income)+age,data=df,
          index=c('state','year'),
          model='within')
summary(reg2)

reg3<-plm(formula=fatalityrate~sb_useage+speed65+speed70+ba08+drinkage21+log(income)+age,data=df,
          index=c('state','year'),
          model='within',
          effect='twoways')
summary(reg3)

pFtest(reg2,reg3)

reg4<-plm(formula=sb_useage~primary+secondary+speed65+speed70+ba08+drinkage21+log(income)+age,data=df,
            index=c('state','year'),
            model='within',
            effect='twoways')
summary(reg4)
