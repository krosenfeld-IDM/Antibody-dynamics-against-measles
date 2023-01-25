#------------- Figure 4 -------------#

library(survival)
library(ggfortify)
library(survminer)

dat_4a<-read.csv("dat_Github_Fig 4A.csv")

km_fit1 <- survfit(Surv(age, Ab) ~ group, data=dat_4a)

myplot1<-ggsurvplot(km_fit1, data = data_final, pval = F,palette = mycols[1:2],xlim=c(0,84),linetype=c(1,1),censor=T,
                   risk.table = T,conf.int=F,break.x.by=12,censor.size=3.5, size = 0.5,fun="event")

survdiff(Surv(age, Ab) ~ group, data = dat_4a)


dat_4b<-read.csv("dat_Github_Fig 4B.csv")

km_fit2 <- survfit(Surv(age, Ab) ~ group, data=dat_4b)

myplot2<-ggsurvplot(km_fit2, data = data_final, pval = F,palette = mycols[1:2],xlim=c(0,84),linetype=c(1,1),censor=T,
                    risk.table = T,conf.int=F,break.x.by=12,censor.size=3.5, size = 0.5,fun="event")

survdiff(Surv(age, Ab) ~ group, data = dat_4b)
