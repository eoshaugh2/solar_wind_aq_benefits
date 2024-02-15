# script to generate Figures 3 and 4

library(tidyverse)

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/")
source("~/Desktop/Data/prep.R")

# point estimates
aeo<-read.csv("Data/wind_solar_shares.csv") %>%  rename(windWt=wind,solarWt=solar) %>%
  mutate(intc=ifelse(region=="TEX","T",
                     ifelse(region=="WECC","W","E"))) 
# bootstrap to pull variances on gen estimates
bta<-read.csv("Outputs/raw_bootstrap.csv") 
# need two separate sets weighted each by AEO
bxa<-bta %>% inner_join(aeo) %>% filter(year==2022)
aggr<-function(z) {
  if (z=="solar") d<-bxa %>% mutate(wt=bxa$solarWt,dNox=dSolarNox,dSo2=dSolarSo2,dCo2=dSolarCo2)
  else d<-bxa %>% mutate(wt=bxa$windWt,dNox=dWindNox,dSo2=dWindSo2,dCo2=dWindCo2)
  a<-as.data.frame(d %>% group_by(iter,y) %>% # then get down to iteration level with weighted means
                  summarize(gen=weighted.mean(wind,wt),
                            nox=weighted.mean(nox,wt),so2=weighted.mean(so2,wt),co2=weighted.mean(co2,wt),
                            dnox=weighted.mean(dnox,wt),dso2=weighted.mean(dso2,wt),dco2=weighted.mean(dco2,wt),
                            DNox=weighted.mean(dNox,wt),DSo2=weighted.mean(dSo2,wt),DCo2=weighted.mean(dCo2,wt))) %>%
    mutate(emnox=nox*dnox,emso2=so2*dso2,emco2=co2*dco2)
  adj<-sqrt(c(mean(a$emnox),mean(a$emso2),mean(a$emco2))) # adjust so that emissions rates and damages are in same units
  adjE<-adj/c(mean(a$nox),mean(a$so2),mean(a$co2))
  adjD<-adj/c(mean(a$dnox),mean(a$dso2),mean(a$dco2))
  b<-a %>% mutate(nox=nox*adjE[1],so2=so2*adjE[2],co2=co2*adjE[3],
                  dnox=dnox*adjD[1],dso2=dso2*adjD[2],dco2=dco2*adjD[3])
  print("MUST BE TRUE")
  print(abs(((mean(a$nox)*mean(a$dnox))-(mean(b$nox)*mean(b$dnox))))<1)
  print(abs((sd(a$dnox)/mean(a$dnox))-(sd(b$dnox)/mean(b$dnox)))<0.01)
  return(b)
}
sla<-aggr("solar")
wna<-aggr("wind")
bsr<-function(z) data.frame(poll=rep(c("nox","so2","co2"),times=2),y=rep(c("coal","gas"),each=3),
                            sd=c(sd(z$DNox[z$y=="coal"]),sd(z$DSo2[z$y=="coal"]),sd(z$DCo2[z$y=="coal"]),
                                 sd(z$DNox[z$y=="gas"]),sd(z$DSo2[z$y=="gas"]),sd(z$DCo2[z$y=="gas"]))) %>%
  mutate(tot=sum(sd)) %>% mutate(share=sd/tot)
slb<-bsr(sla)
wnb<-bsr(wna)

# calculate all incremental contributions
cln<-function(z,zz) {
  a<-as.data.frame(z %>% group_by(y) %>%
                       summarize(gen=sd(gen),nox=sd(nox),so2=sd(so2),co2=sd(co2),
                                 dnox=sd(dnox),dso2=sd(dso2),dco2=sd(dco2)))
  b<-data.frame(poll=rep(c("nox","so2","co2"),each=2),y=rep(c("coal","gas"),times=3)) %>%
    inner_join(select(zz,poll,y,share)) %>%
    mutate(gen=rep(a$gen,times=3),
           em=c(a$nox,a$so2,a$co2),
           dm=c(a$dnox,a$dso2,a$dco2)) %>%
    mutate(tot=gen+em+dm) %>% mutate(gp=(gen/tot)*share,ep=(em/tot)*share,dp=(dm/tot)*share)
  return(b)
}
slz<-cln(sla,slb)
wnz<-cln(wna,wnb)

# down to categories in % terms
fnl<-function(z) {
  l<-list()
  l[[1]]<-aggregate(gp~y,data=z,sum)
  l[[2]]<-as.data.frame(z %>% mutate(pollu=ifelse(poll=="co2","co2","noxsox")) %>%
                       group_by(pollu) %>%
                       summarize(ep=sum(ep),dp=sum(dp))) 
  return(l)
}
slf<-fnl(slz)
wnf<-fnl(wnz)
# really not very interesting, it is so dominated by co2 damages


# another way to break it down is one step up, y+poll
fgd<-function(z) {
  a<-data.frame(z %>% mutate(pollu=ifelse(poll=="co2","co2","noxsox")) %>%
                     group_by(y,pollu) %>%
                     summarize(share=sum(share),gp=sum(gp),ep=sum(ep),dp=sum(dp))) %>%
    arrange(share) %>% mutate(x=c(1:4)) %>%
    inner_join(data.frame(y=rep(c("coal","gas"),each=2),pollu=rep(c("co2","noxsox"),times=2),
                          lab=c("Coal: CO2","Coal: NOx/SOx","Gas: CO2","Gas: NOx/SOx")))
  b<-data.frame(x=rep(a$x,times=3),lab=rep(a$lab,times=3),
                fl=rep(c("aGen","bEm","cDm"),each=4),
                y=c(a$gp,a$ep,a$dp))
  return(b)
}
fslr<-fgd(slz)
fwnd<-fgd(wnz)
fgr<-function(z,ttl) {
  base(z)+geom_col(aes(x,y*100,fill=fl),width=0.7)+ggtitle(ttl)+
    scale_fill_manual(values=c("firebrick4","lightblue3","navy"),
                      name="Uncertainty Source",labels=c("Generation","Emissions","Damages"))+
    coord_flip()+theme(axis.title.y=element_blank(),
                       legend.position=c(0.75,0.14))+
    ylab("Uncertainty Contribution (%)")+
    scale_x_continuous(breaks=c(1:4),labels=unique(z$lab))
}
ggsave("Figures/uncertainty_stacks.jpg",
       plot=plot_grid(fgr(fwnd,"Wind"),
                      fgr(fslr,"Solar")),width=12,height=6)


# just NOx/SOx
# need two separate sets weighted each by AEO
bsr<-function(z) data.frame(poll=rep(c("nox","so2"),times=2),y=rep(c("coal","gas"),each=2),
                            sd=c(sd(z$DNox[z$y=="coal"]),sd(z$DSo2[z$y=="coal"]),
                                 sd(z$DNox[z$y=="gas"]),sd(z$DSo2[z$y=="gas"]))) %>%
  mutate(tot=sum(sd)) %>% mutate(share=sd/tot)
slb<-bsr(sla)
wnb<-bsr(wna)

# calculate all incremental contributions
cln<-function(z,zz) {
  a<-as.data.frame(z %>% group_by(y) %>%
                     summarize(gen=sd(gen),nox=sd(nox),so2=sd(so2),
                               dnox=sd(dnox),dso2=sd(dso2)))
  b<-data.frame(poll=rep(c("nox","so2"),each=2),y=rep(c("coal","gas"),times=2)) %>%
    inner_join(select(zz,poll,y,share)) %>%
    mutate(gen=rep(a$gen,times=2),
           em=c(a$nox,a$so2),
           dm=c(a$dnox,a$dso2)) %>%
    mutate(tot=gen+em+dm) %>% mutate(gp=(gen/tot)*share,ep=(em/tot)*share,dp=(dm/tot)*share)
  return(b)
}
slz<-cln(sla,slb)
wnz<-cln(wna,wnb)

# down to categories in % terms
fnl<-function(z) {
  a<-aggregate(gp~y,data=z,sum)
  b<-as.data.frame(z %>% group_by(poll) %>%
                          summarize(ep=sum(ep),dp=sum(dp))) 
  c<-data.frame(lab=c("Coal avoided (MWh/MWh)","Gas avoided (MWh/MWh)",
                      "NOx emissions rate (kg/MWh)","SO2 emissions rate (kg/MWh)",
                      "Value of avoided NOx ($/kg)","Value of avoided SO2 ($/kg)"),
                fl=rep(c("aGen","bEm","cDm"),each=2),
                y=c(a$gp,b$ep,b$dp)) %>%
    arrange(y) %>% mutate(x=c(1:6))
  return(c)
}
slf<-fnl(slz)
wnf<-fnl(wnz)
fgr<-function(z,ttl) {
  base(z)+geom_col(aes(x,y*100,fill=fl),width=0.7)+ggtitle(ttl)+
    scale_fill_manual(values=c('firebrick4','lightblue3','navy'),guide="none")+
    coord_flip()+theme(axis.title.y=element_blank())+
    ylab("Uncertainty Contribution (%)")+
    scale_x_continuous(breaks=c(1:6),labels=z$lab)
}
ggsave("Figures/uncertainty_noxsox.jpg",
       plot=plot_grid(fgr(wnf,"Wind"),
                      fgr(slf,"Solar")),width=12,height=4)

