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

# first generate a national-level file based on generation weights
bxa<-bta %>% inner_join(aeo) %>% filter(year==2022)
aggr<-function(z) {
  if (z=="solar") d<-bxa %>% mutate(wt=bxa$solarWt,dNox=dSolarNox,dSo2=dSolarSo2,dCo2=dSolarCo2)
  else d<-bxa %>% mutate(wt=bxa$windWt,dNox=dWindNox,dSo2=dWindSo2,dCo2=dWindCo2)
  a<-as.data.frame(d %>% group_by(iter,y) %>% # then get down to iteration level with weighted means
                  summarize(gen=weighted.mean(wind,wt),
                            nox=weighted.mean(nox,wt),so2=weighted.mean(so2,wt),co2=weighted.mean(co2,wt),
                            dnox=weighted.mean(dnox,wt),dso2=weighted.mean(dso2,wt),dco2=weighted.mean(dco2,wt),
                            DNox=weighted.mean(dNox,wt),DSo2=weighted.mean(dSo2,wt),DCo2=weighted.mean(dCo2,wt)))
  return(a)
}
sla<-aggr("solar")
wna<-aggr("wind")

# from those files, pull standard deviations and means of generation (G), emissions rates (E), and damage rates (D)
clnr<-function(dd) {
  a<-dd %>% mutate(BNox=DNox,BSo2=DSo2,BCo2=DCo2)
  b<-as.data.frame(a %>% mutate(g2=gen,n2=nox,dn2=dnox,Dn2=DNox,s2=so2,ds2=dso2,Ds2=DSo2,c2=co2,dc2=dco2,Dc2=DCo2) %>% group_by(y) %>%
                  summarize(gen=mean(gen),vGen=var(g2),
                            nox=mean(nox),vNox=var(n2),so2=mean(so2),vSo2=var(s2),co2=mean(co2),vCo2=var(c2),
                            dnox=mean(dnox),vDnox=var(dn2),dso2=mean(dso2),vDso2=var(ds2),dco2=mean(dco2),vDco2=var(dc2),
                            bNox=mean(DNox),vBnox=var(BNox),bSo2=mean(DSo2),vBso2=var(BSo2),bCo2=mean(DCo2),vBCo2=var(BCo2))) %>%
    mutate(ZNox=(bNox^2)/vBnox,ZSo2=(bSo2^2)/vBso2,ZCo2=(bCo2^2)/vBCo2) # a constant for estimating the contributions
  return(b)
}
slz<-clnr(sla)
wnz<-clnr(wna)

# contributions are equal to Z*Var(F)/F^2, where F is the factor, see Procedures
cntr<-function(dd) {
  a<-dd %>% mutate(cgenNox=ZNox*vGen/gen^2,cgenSo2=ZSo2*vGen/gen^2,cgenCo2=ZCo2*vGen/gen^2,
                   cnox=ZNox*vNox/nox^2,cso2=ZSo2*vSo2/so2^2,cco2=ZCo2*vCo2/co2^2,
                   cDnox=ZNox*vDnox/dnox^2,cDso2=ZSo2*vDso2/dso2^2,cDco2=ZCo2*vDco2/dco2^2) 
  # there are six components: 3 pollutants for each fuel type, but combine noxsox to make 4
  b<-a %>% mutate(cgennoxsox=cgenNox+cgenSo2,cnoxsox=cnox+cso2,cDnoxsox=cDnox+cDso2)
  # to convert to % shares, gen needs to be multiplied by 2 for noxsox to reflect it's role in each component
  c<-b %>% 
    mutate(totNS=cgennoxsox+cnoxsox+cDnoxsox,totCo2=cgenCo2+cco2+cDco2) %>%# %>% # get to sum to 1 (does not perfectly sum to 1 due
    mutate(pgenNS=cgennoxsox/totNS,pgenCo2=cgenCo2/totCo2,
           pnoxsox=cnoxsox/totNS,pco2=cco2/totCo2, # to random correlation between variables)
           pDnoxsox=cDnoxsox/totNS,pDco2=cDco2/totCo2)
  # at this point you have the % contributions within each component, those now need to be converted to
  # % contributions to total variation based on the sum of the four variances
  d<-c %>% mutate(vBnoxsox=vBnox+vBso2) %>% mutate(totV=sum(vBnoxsox)+sum(vBCo2)) %>%
    mutate(noxsoxVshare=vBnoxsox/totV,co2Vshare=vBCo2/totV) %>%
    mutate(pgenNS=pgenNS*noxsoxVshare,pgenCo2=pgenCo2*co2Vshare,
           pnoxsox=pnoxsox*noxsoxVshare,pco2=pco2*co2Vshare, 
           pDnoxsox=pDnoxsox*noxsoxVshare,pDco2=pDco2*co2Vshare)
  return(d)
}
slc<-cntr(slz)
wnc<-cntr(wnz)

# reshape for figure
fgr<-function(dd) {
  a<-data.frame(typ=c(rep(c("Coal: CO2","Gas: CO2"),times=3),rep(c("Coal: NOx/SOx","Gas: NOx:SOx"),times=3)),
                y=c(dd$pgenCo2,dd$pco2,dd$pDco2,
                    dd$pgenNS,dd$pnoxsox,dd$pDnoxsox),
                fl=rep(rep(c("aGen","bEm","cDmg"),each=2),times=2)) # doubling gen to account for it showing up twice in combined Nox and Sox components
  b<-aggregate(y~typ,data=a,sum) %>% arrange(-y) %>% mutate(x=c(4:1))
  c<-inner_join(a,dplyr::select(b,typ,x)) %>% arrange(x)
  return(c)
}
fslr<-fgr(slc)
fwnd<-fgr(wnc)


fgr<-function(z,ttl) {
  base(z)+geom_col(aes(x,y*100,fill=fl),width=0.7)+ggtitle(ttl)+
    scale_fill_manual(values=c("firebrick4","lightblue3","navy"),
                      name="Variation Source",labels=c("Generation","Emissions","Damages"))+
    coord_flip()+theme(axis.title.y=element_blank(),
                       legend.position=c(0.75,0.14))+
    ylab("Contribution to Variation (%)")+
    scale_x_continuous(breaks=c(1:4),labels=unique(z$typ))
}
ggsave("Figures/uncertainty_stacks.jpg",
       plot=plot_grid(fgr(fwnd,"Wind"),
                      fgr(fslr,"Solar")+theme(legend.position=c(90,90))),width=12,height=6)


# just NOx/SOx
lmcntr<-function(dd) {
  a<-dd %>% mutate(cgenNox=ZNox*vGen/gen^2,cgenSo2=ZSo2*vGen/gen^2,
                   cnox=ZNox*vNox/nox^2,cso2=ZSo2*vSo2/so2^2,
                   cDnox=ZNox*vDnox/dnox^2,cDso2=ZSo2*vDso2/dso2^2) 
  # to convert to % shares, gen needs to be multiplied by 2 for noxsox to reflect it's role in each component
  b<-a %>% 
    mutate(totNox=cgenNox+cnox+cDnox,totSo2=cgenSo2+cso2+cDso2) %>%# %>% # get to sum to 1 (does not perfectly sum to 1 due
    mutate(pgennox=cgenNox/totNox,pgenSo2=cgenSo2/totSo2,
           pnox=cnox/totNox,pso2=cso2/totSo2, # to random correlation between variables)
           pDnox=cDnox/totNox,pDso2=cDso2/totSo2)
  # at this point you have the % contributions within each component, those now need to be converted to
  # % contributions to total variation based on the sum of the four variances
  c<-b %>% mutate(totV=sum(vBnox)+sum(vBso2)) %>%
    mutate(noxVshare=vBnox/totV,so2Vshare=vBso2/totV) %>%
    mutate(pgennox=pgennox*noxVshare,pgenSo2=pgenSo2*so2Vshare,
           pnox=pnox*noxVshare,pso2=pso2*so2Vshare, 
           pDnox=pDnox*noxVshare,pDso2=pDso2*so2Vshare)
  return(c)
}
lmslc<-lmcntr(slz)
lmwnc<-lmcntr(wnz)

# down to categories in % terms
fnl<-function(dd) {
  c<-data.frame(lab=c("Coal avoided (MWh/MWh)","Gas avoided (MWh/MWh)",
                      "NOx emissions rate (kg/MWh)","SO2 emissions rate (kg/MWh)",
                      "Value of avoided NOx ($/kg)","Value of avoided SO2 ($/kg)"),
                fl=rep(c("aGen","bEm","cDm"),each=2),
                y=c(dd$pgennox+dd$pgenSo2,sum(dd$pnox),sum(dd$pso2),sum(dd$pDnox),sum(dd$pDso2))) %>%
    arrange(y) %>% mutate(x=c(1:6))
  return(c)
}
slf<-fnl(lmslc)
wnf<-fnl(lmwnc)
fgr<-function(z,ttl) {
  base(z)+geom_col(aes(x,y*100,fill=fl),width=0.7)+ggtitle(ttl)+
    scale_fill_manual(values=c('firebrick4','lightblue3','navy'),guide="none")+
    coord_flip()+theme(axis.title.y=element_blank())+
    ylab("Contribution to Variation (%)")+
    scale_x_continuous(breaks=c(1:6),labels=z$lab)
}
ggsave("Figures/uncertainty_noxsox.jpg",
       plot=plot_grid(fgr(wnf,"Wind"),
                      fgr(slf,"Solar")),width=12,height=4)
