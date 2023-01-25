#------------- Figure 3 -------------#

library(ggeffects)
library(splines)

dat<-read.csv("dat_Github_Fig 3.csv")

dat$MCV_group<-NA
dat$MCV_group[dat$vax_status==0]<-0
dat$MCV_group[dat$vax_status!=0]<-1

feff.log<-bam(log_titer~s(age,k=3,by=MCV_group)+vax_status+s(kid, bs = "re"),
              family = gaussian(link = "identity"),data = dat)

new.dat<-data.frame(vax_status= c(rep(0,9),rep(1,10),rep(2,42)),
                    MCV_group=c(rep(0,9),rep(1,52)),
                    age= seq(0,60,1),
                    kid = rep(data_final$kid[1],61))

pre.curv<-predict(feff.log,newdata = new.dat,type = "response",se.fit = T)

data.curv<-data.frame(mean=pre.curv$fit,
                      lower=pre.curv$fit-1.96*pre.curv$se.fit,
                      upper=pre.curv$fit+1.96*pre.curv$se.fit,
                      x_axis=seq(0,60,1))

ggplot(data=data.curv) +
  geom_ribbon(data=data.curv,aes(x=x_axis,ymin = lower, ymax = upper),fill="#6BB1CB",alpha=0.4)+
  geom_line(data=data.curv,aes(x=x_axis,y=mean),color="#302F5F",size=0.5)+
  coord_cartesian(xlim=c(0,60.2),ylim=c(2,10), clip = "off")+
  theme_bw()