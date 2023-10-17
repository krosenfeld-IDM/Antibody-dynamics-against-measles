#------------- Figure 2 -------------#

library(ggeffects)
library(splines)
library(ggplot2)
library(mgcv)

dat<-read.csv("dat_Github_Fig 2.csv")

feff.log<-bam(log_titer~s(age,k=4)+vax_status+s(kid, bs = "re"),
              family = gaussian(link = "identity"),data = dat)

new.dat<-data.frame(vax_status= c(rep(0,9),rep(1,16)),
                    age= seq(0,24,1),
                    kid= rep(dat$kid[1],25))

pre.curv<-predict(feff.log,newdata = new.dat,type = "response",se.fit = T)

data.curv<-data.frame(mean=pre.curv$fit,
                      lower=pre.curv$fit-1.96*pre.curv$se.fit,
                      upper=pre.curv$fit+1.96*pre.curv$se.fit,
                      x_axis=seq(0,24,1))

ggplot(data=data.curv) +
  geom_ribbon(data=data.curv,aes(x=x_axis,ymin = lower, ymax = upper),fill="#6BB1CB",alpha=0.4)+
  geom_line(data=data.curv,aes(x=x_axis,y=mean),color="#302F5F",size=0.5)+
  coord_cartesian(xlim=c(0.0,24.0),ylim=c(2,10), clip = "off")+
  theme_bw()
