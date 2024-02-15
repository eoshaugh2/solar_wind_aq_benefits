#

library(tidyverse)
library(lubridate)
an<-function(z) as.numeric(z)

# define WD where damages data are stod
setwd("USER_INPUT/")
WDdta<-"USER_INPUT" # WD where output will be saved


# 1) EPA
lta <- read.csv("Data/damages/EPA-Benefits-per-ton-files/2025_bpt_pm-low-dis3-Raw-Reformat-ShortTons-2022Dolars.csv")
hta <- read.csv("Data/damages/EPA-Benefits-per-ton-files/2025_bpt_pm-high-dis3-Raw-Reformat-ShortTons-2022Dolars.csv")
ozl <- read.csv("Data/damages/EPA-Benefits-per-ton-files/2025_bpt_o3-short-dis3-Raw-Reformat-ShortTons-2022Dolars.csv")
ozh <- read.csv("Data/damages/EPA-Benefits-per-ton-files/2025_bpt_o3-long-dis3-Raw-Reformat-ShortTons-2022Dolars.csv")

##### tangent here to estimate the % of NOx that is emitted during ozone season
# EIA monthly gen data ~ https://www.eia.gov/electricity/data.php#sales, 2022
eia <- read.csv("Data/damages/eia_923_monthly_2022.csv") %>%
  filter(type=="Total Electric Power Industry",source%in%c("Coal","Natural Gas"),state!="US-Total") %>%
  mutate(ozs=ifelse(month%in%c(5:9),1,0)) 
eza<-as.data.frame(eia %>% group_by(state,ozs,source) %>% summarize(mwh=sum(mwh))) 

# eGrid plant-level
eta <- read.csv("Data/egrid_plant_level.csv") %>% select(PSTATABB,PLFUELCT,PLNGENAN,PLNOXRTA)
colnames(eta)<-c("state","fuel","gen","nox")
eta<-eta %>% filter(fuel%in%c("COAL","GAS")) %>%
  mutate(gen=gsub(",","",gen)) %>%
  mutate(gen=as.numeric(as.character(gen)),nox=as.numeric(as.character(nox))) %>%
  mutate(gen=ifelse(is.na(gen),0,gen),nox=ifelse(is.na(nox),0,nox))
ema<-as.data.frame(eta %>% group_by(state,fuel) %>% summarize(nox=weighted.mean(nox,gen))) %>%
  inner_join(data.frame(fuel=c("COAL","GAS"),source=c("Coal","Natural Gas"))) %>%
  filter(!is.nan(nox))

# bring together
adj<-left_join(eza,ema) %>%
  mutate(nox=ifelse( (is.nan(nox) | is.na(nox)) & source=="Coal",mean(ema$nox[ema$source=="Coal"]),
                    ifelse( (is.nan(nox) | is.na(nox)) & source=="Natural Gas",mean(ema$nox[ema$source=="Natural Gas"]),nox))) %>%
  mutate(tNox=mwh*nox) %>% inner_join(aggregate(tNox~state+source,data=.,sum) %>% rename(tot=tNox)) %>%
  mutate(pct=tNox/tot) %>% filter(ozs==1)
ozAdj<-function(z) rename(z,state=State.Region) %>% left_join(select(adj,source,state,pct)) %>%
  mutate(pct=ifelse(is.nan(pct),0,pct)) %>%
  mutate(ozNox=NOX*pct) %>% mutate(ifelse(is.nan(ozNox),0,ozNox)) %>% select(state,source,ozNox)
ozz<-inner_join(ozAdj(ozl) %>% rename(ozNox_lo=ozNox),
                ozAdj(ozh) %>% rename(ozNox_hi=ozNox))

# bring together with the other estimates
epa<-inner_join(rename(lta,state=State.Region,nox_lo=NOx,so2_lo=SO2) %>% select(state,nox_lo,so2_lo),
                rename(hta,state=State.Region,nox_hi=NOx,so2_hi=SO2) %>% select(state,nox_hi,so2_hi)) %>%
  left_join(ozz) %>%
  mutate(nox_lo=nox_lo+ozNox_lo,nox_hi=nox_hi+ozNox_hi)

# eGrid for state-level weighting factors
egr <- read.csv("Data/egrid_plant_level.csv") %>%
  rename(state=PSTATABB) %>% mutate(coal=ifelse(PLFUELCT=="COAL",1,0),fips=paste(str_pad(FIPSST,pad="0",2),str_pad(FIPSCNTY,pad="0",3),sep=""))#
gn<-matrix(c(egr$PLGENACL,egr$PLGENAOL,egr$PLGENAGS,egr$PLGENAOF),nrow=nrow(egr),byrow=F)
gn<-gsub(",","",gn)
gn<-gsub('\\(','-',gn)
gn<-gsub('\\)','',gn)
gn[gn==""]<-0
gna<-as.data.frame(gn) %>% mutate(coalgen=an(V1),gasgen=an(V3),ffgen=an(V1)+an(V2)+an(V3)+an(V4))
sga<-epa %>% left_join(as.data.frame(egr %>% mutate(ffgen=gna$ffgen,coalgen=gna$coalgen,gasgen=gna$ffgen) %>% 
                     mutate(ffgen=ifelse(ffgen<0,0,ffgen),coalgen=ifelse(coalgen<0,0,coalgen),gasgen=ifelse(gasgen<0,0,gasgen)) %>%
                     group_by(state) %>% summarize(ffgen=sum(ffgen),coalgen=sum(coalgen),gasgen=sum(gasgen)))) %>%
  filter(state!="TB") %>%
  mutate(wt=ifelse(source=="Coal",coalgen,gasgen))

# pull weighted damages from each set for each model
inta<-as.data.frame(sga %>%
    mutate(intc=ifelse(state=="TX","T",
                       ifelse(state%in%c("CA","AZ","NM","WA","OR","ID","MT","NV","UT","CO","WY"),"W","E"))) %>% 
      group_by(intc,source) %>% 
      summarize(nox_lo=weighted.mean(nox_lo,wt,na.rm=T),nox_hi=weighted.mean(nox_hi,wt,na.rm=T),
                ozNox_lo=weighted.mean(ozNox_lo,na.rm=T),ozNox_hi=weighted.mean(ozNox_hi,na.rm=T),
                so2_lo=weighted.mean(so2_lo,coalgen),so2_hi=weighted.mean(so2_hi,coalgen)))
epaXpt<-inta %>% 
  mutate(nox_lo=nox_lo/0.907,nox_hi=nox_hi/0.907,ozNox_lo=ozNox_lo/0.907,ozNox_hi=ozNox_hi/0.907,so2_lo=so2_lo/0.907,so2_hi=so2_hi/0.907) # converted to metric tons
#write.csv(epaXpt,"~/Desktop/AQ/Outputs/epa_regional_damage_factors_6_22_23.csv",row.names=F)

# 2) ACS and Harvard 6-cities studies
acs <- read.csv("Data/damages/ACS-12.63VSL-2022Dol.csv")
hvd <- read.csv("Data/damages/H6C-12.63VSL-2022Dol.csv")

# eGrid for county-level weighting factors
ega<-as.data.frame(egr %>% mutate(ffgen=gna$ffgen,coalgen=gna$coalgen) %>% 
                     mutate(ffgen=ifelse(ffgen<0,0,ffgen),coalgen=ifelse(coalgen<0,0,coalgen)) %>%
                     group_by(fips) %>% summarize(ffgen=sum(ffgen),coalgen=sum(coalgen)))

# function to pull weighted damages from each set for each model
pll<-function(zta) {
  dd<-zta %>% mutate(fips=str_pad(fips,pad="0",5)) %>% inner_join(ega) %>%
    mutate(intc=ifelse(state_abbr=="TX","T",
                       ifelse(state_abbr%in%c("CA","AZ","NM","WA","OR","ID","MT","NV","UT","CO","WY"),"W","E"))) 
  so2<-as.data.frame(dd %>% filter(pollutant=="so2") %>% group_by(intc,model) %>% summarize(damage=weighted.mean(damage,coalgen))) %>% mutate(pollutant="so2")
  nox<-as.data.frame(dd %>% filter(pollutant=="nox") %>% group_by(intc,model) %>% summarize(damage=weighted.mean(damage,ffgen))) %>% mutate(pollutant="nox")
  return(rbind(so2,nox))
}
acsa<-pll(acs)
hvda<-pll(hvd)

xpt<-rbind(acs %>% mutate(set='acs'),hvd %>% mutate(set='hvd'))
#write.csv(xpt,"~/Desktop/AQ/Outputs/weighted_damages_2ests_5_19_23.csv",row.names=F)


# export the SE approach
achv<-rbind(acsa %>% mutate(cl="b",x=rep(rep(c(1,2,3),each=3),times=2)),
            hvda %>% mutate(cl="c",x=rep(rep(c(1.2,2.2,3.2),each=3),times=2)))
acshvd<-rbind(mutate(achv,source="Coal"),mutate(achv,source="Natural Gas"))

fta<-data.frame(model="0epa",cl="a",pollutant=rep(c("nox","so2"),each=12),
                      intc=rep(c("E","E","T","T","W","W"),times=4),source=rep(epaXpt$source,times=4),
                      x=rep(c(0.8,0.8,1.8,1.8,2.8,2.8),times=4),damage=c(epaXpt$nox_lo,epaXpt$nox_hi,epaXpt$so2_lo,epaXpt$so2_hi))
tla<-rbind(acshvd,fta)

# keep high-end o3 estimate for the ACS/H6 studies
ozt<-select(epaXpt,intc,source,ozNox_hi)
ozb<-aggregate(ozNox_hi~intc,data=ozt,mean)

toa<-tla %>% inner_join(ozb) %>%
  mutate(damage=ifelse(pollutant=="nox" & model!="0epa",damage+ozNox_hi,damage))
wta<-inner_join(aggregate(damage~pollutant+intc+source,data=toa,mean),
               aggregate(damage~pollutant+intc+source,data=toa,sd) %>% rename(sd=damage)) %>%
  mutate(se=sd/sqrt(8)) # 8 model variations

# get into the same units
wta<-wta %>% mutate(damage=ifelse(pollutant%in%c("nox","so2"),damage*10^-3,damage),
                    sd=ifelse(pollutant%in%c("nox","so2"),sd*10^-3,sd),
                    se=ifelse(pollutant%in%c("nox","so2"),se*10^-3,se))
#write.csv(wta,"~/Desktop/AQ/Outputs/weighted_damage_factors_6_22_23.csv",row.names=F)

# CO2 assumptions ~ mean is $185, 95% between 44 and 413, so a skewed distribution
library(sn)
infl<-1.105 # inflation factor to time-adjust co2 values
co2asm<-c(185,115.55)*infl # assumed "mean" and SD for co2
xx<-cp2dp(c(co2asm[1],co2asm[2],0.995),"SN")
qsn(0.05,xi=xx[1], omega=xx[2], alpha=xx[3]) # this roughly achieves it
qsn(0.95,xi=xx[1], omega=xx[2], alpha=xx[3])
clnr<-function(z) data.frame(intc=unique(wta$intc),source=z,nox=wta$damage[wta$pollutant=="nox" & wta$source==z],noxSd=wta$se[wta$pollutant=="nox" & wta$source==z],
                             so2=wta$damage[wta$pollutant=="so2" & wta$source==z],so2Sd=wta$se[wta$pollutant=="so2" & wta$source==z],
                             co2=co2asm[1],co2sd=co2asm[2])
wwa<-rbind(clnr("Coal"),clnr("Natural Gas"))
setwd(wDdta)
write.csv(wwa,"weighted_damage_factors.csv",row.names=F)

