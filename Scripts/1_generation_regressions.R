# script to run regressions, export coefficients and SE

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/") 

library(tidyverse)
library(fixest)
library(sandwich)

an<-function(z) as.numeric(z)
dta <- read.csv("Data/analysis_set.csv") 
# wind/solar shares (for cutoffs)
lta<-as.data.frame(dta %>% mutate(year=year(as.Date(utc_date))) %>% group_by(region,year) %>%
                     summarize(load=sum(load),wind=sum(wind),solar=sum(solar)) %>%
                     mutate(wind_pct=wind/load,solar_pct=solar/load))
write.csv(lta,"Data/wind_solar_shares.csv",row.names=F)
ixxa<-read.csv("Data/trading_regions.csv") %>% rename(region=Region)
rgnz<-data.frame(region=sort(unique(dta$region)))

q1<-function(z) {
  a<-quantile(z,.01,na.rm=T) # need to write this way in case the q1 is negative
  if (a<0) b<-a*10
  else b<-a/2
  return(b)
} 
q99<-function(z) {
  a<-quantile(z,.99,na.rm=T) # need to write this way in case the q99 is negative
  if (a<0) b<-quantile(rta$ix,.99,na.rm=T)*10 # this is the only one that can be negative, use population q99
  else b<-10*a
  return(b)
} 
rta<-dta %>% inner_join(rgnz) %>% mutate(date=as.Date(utc_date)) %>%
  mutate(month=month(date),year=year(date),wkday=wday(date)) %>% filter(year>=2019) %>%
  mutate(my=paste(month,year),hWk=paste(utc_hour,wkday))%>% 
  mutate(ix=-ix,wwind=wind,ssolar=solar,hix=-ix) # make it net IMPORTS
# note the variables wwind and ssolar are clones of wind and solar, they are only generated 
# for convenience to remove wind/solar from certain regression outputs (see mdlr)
rga<-list()
for (i in 1:nrow(rgnz)) rga[[i]]<-filter(rta,region==rgnz$region[i]) %>%
  mutate(coal=ifelse(coal>q99(coal),NA,coal),gas=ifelse(gas>q99(gas),NA,gas),hydro=ifelse(hydro>q99(hydro),NA,hydro),
         ix=ifelse(ix<q1(ix) | ix>q99(ix),NA,ix),
         load=ifelse(load>q99(load),NA,load),solar=ifelse(solar>q99(solar),NA,solar),wind=ifelse(wind>q99(wind),NA,wind),
         exLoad=ifelse(exLoad>q99(exLoad),NA,exLoad),exSolar=ifelse(exSolar>q99(exSolar),NA,exSolar),exWind=ifelse(exWind>q99(exWind),NA,exWind))


# create opposite regions for each (except ERCOT and WECC)
opp<-list()
for (i in 1:(nrow(rgnz)-2)) opp[[i]]<-as.data.frame(filter(rta,region%in%ixxa$DIBA_Region[ixxa$region==rgnz$region[i]],region!=rgnz$region[i]) %>%
                                                      rename(DIBA_Region=region) %>% inner_join(filter(ixxa,region==rgnz$region[i])) %>%
                                                      mutate(coal=ifelse(coal>q99(coal),NA,coal*wt),gas=ifelse(gas>q99(gas),NA,gas*wt),hydro=ifelse(hydro>q99(hydro),NA,hydro*wt),
                                                             ix=ifelse(ix<q1(ix) | ix>q99(ix),NA,ix*wt),
                                                             load=ifelse(load>q99(load),NA,load*wt),solar=ifelse(solar>q99(solar),NA,solar*wt),wind=ifelse(wind>q99(wind),NA,wind*wt),
                                                             exLoad=ifelse(exLoad>q99(exLoad),NA,exLoad*wt),exSolar=ifelse(exSolar>q99(exSolar),NA,exSolar*wt),exWind=ifelse(exWind>q99(exWind),NA,exWind*wt)) %>%
                                                      group_by(date,utc_hour) %>% summarize(gas=sum(gas),coal=sum(coal),solar=sum(solar),wind=sum(wind),load=sum(load),hydro=sum(hydro),ix=sum(ix))) %>%
  inner_join(rga[[i]] %>% select(date,utc_hour,solar,wind,load) %>% rename(xSolar=solar,xWind=wind,xLoad=load)) %>% mutate(date=as.Date(date)) %>%
  mutate(month=month(date),year=year(date),wkday=wday(date),wwind=wind,ssolar=solar,hix=ix) %>% mutate(my=paste(month,year),hWk=paste(utc_hour,wkday))

# adjustment for NY-Ontario trade
iesoW<-1-sum(ixxa$wt[ixxa$region=="NY"])
ieso<-read.csv("Data/ieso_clean.csv") %>% mutate(utc_date=as.Date(utc_date)) %>% 
  rename(date=utc_date,ieso_gas=gas,ieso_wind=wind,ieso_solar=solar,ieso_load=load,ieso_ix=ix,ieso_hydro=hydro) %>% 
  mutate(ieso_ix=-ieso_ix) %>% # converted to net imports
  select(-local_hour,-local_date)
opp[[7]]<-opp[[7]] %>% left_join(ieso) %>% mutate(gas=gas+ieso_gas*iesoW,wind=wind+ieso_wind*iesoW,solar=solar+ieso_solar*iesoW,load=load+ieso_load*iesoW,ix=ix+ieso_ix*iesoW)

# regressions
rs<-c("wind","solar","hydro")
rx<-c("wind","solar","hydro","ix")
mdlr<-function(z,yr) {
  dda<-filter(rga[[z]],year==yr,!is.na(ix),!is.na(exLoad),!is.na(exSolar),!is.na(exWind))  
  dda$ixx<-predict(lm(ix~exLoad+exSolar+exWind,data=dda))
  if (rgnz$region[z]%in%c("TEX","WECC")) oppa<-NULL
  else oppa<-filter(opp[[z]],year==yr)
  if (rgnz$region[z]=="NY") {
    a<-feols(gas~wind+load+ixx|my+hWk,data=dda,se='hetero')
    c<-feols(gas~wwind+load+hydro+ixx|my+hWk,data=dda,se='hetero')
    e<-feols(hydro~wind+load+ixx|my+hWk,data=dda,se='hetero')
  } else {
    if (rgnz$region[z]%in%c("TEX","WECC")) {
      a<-feols(gas~wind+solar+load|my+hWk,data=dda,se='hetero')
      b<-feols(coal~wind+solar+load|my+hWk,data=dda,se='hetero')
      c<-feols(gas~wwind+ssolar+hydro+load|my+hWk,data=dda,se='hetero')
      d<-feols(coal~wwind+ssolar+hydro+load|my+hWk,data=dda,se='hetero')
      e<-feols(hydro~wind+solar+load|my+hWk,data=dda,se='hetero')
    } else {
      a<-feols(gas~wind+solar+load+ixx|my+hWk,data=dda,se='hetero')
      b<-feols(coal~wind+solar+load+ixx|my+hWk,data=dda,se='hetero')
      c<-feols(gas~wwind+ssolar+hydro+load+ixx|my+hWk,data=dda,se='hetero')
      d<-feols(coal~wwind+ssolar+hydro+load+ixx|my+hWk,data=dda,se='hetero')
      e<-feols(hydro~wind+solar+load+ixx|my+hWk,data=dda,se='hetero')
    } 
  }
  cpl<-function(yy,dd,rr) {
    nwSes<-sqrt(diag(NeweyWest(dd)))
    rzz<-data.frame(y=yy,x=names(dd$coefficients[names(dd$coefficients)%in%rr]),
                    coeff=an(dd$coefficients[names(dd$coefficients)%in%rr]),
                    se=an(nwSes[names(nwSes)%in%rr])) # Newey-West
    return(rzz)
  } 
  if (!(rgnz$region[z]%in%c("NE","NY","TEX","WECC"))) {
    h<-feols(gas~wwind+ssolar+hydro+load+hix|my+hWk,data=oppa,se='hetero')
    i<-feols(coal~wwind+ssolar+hydro+load+hix|my+hWk,data=oppa,se='hetero')
    j<-feols(ix~wind+solar+load+exWind+exSolar+exLoad|my+hWk,data=dda,se='hetero')
    k<-feols(hydro~wwind+ssolar+load+ix|my+hWk,data=oppa,se='hetero')
    rz<-rbind(cpl('gas',a,rs),cpl('coal',b,rs),cpl('gas',c,rs),cpl('coal',d,rs),cpl('hydro',e,rs),
              cpl('int_gas',h,rx),cpl('int_coal',i,rx),cpl('int_ix',j,rx),cpl('int_hydro',k,rx)) %>%
      mutate(region=rgnz$region[z]) %>% select(region,y,x,coeff,se) 
  } else {
    if (rgnz$region[z]%in%c("NE","NY")) {
      h<-feols(gas~wwind+ssolar+load+hydro+hix|my+hWk,data=oppa,se='hetero')
      j<-feols(ix~wind+solar+load+exSolar+exWind+exLoad|my+hWk,data=dda)
      k<-feols(hydro~wwind+ssolar+load+ix|my+hWk,data=oppa,se='hetero')
      rz<-rbind(cpl('gas',a,rs),cpl('gas',c,rs),cpl('hydro',e,rs),
                cpl('int_gas',h,rx),cpl('int_ix',j,rx),cpl('int_hydro',k,rx)) %>%
        mutate(region=rgnz$region[z]) %>% select(region,y,x,coeff,se) 
    } else {
      rz<-rbind(cpl('gas',a,rs),cpl('coal',b,rs),cpl('gas',c,rs),cpl('coal',d,rs),cpl('hydro',e,rs)) %>%
        mutate(region=rgnz$region[z]) %>% select(region,y,x,coeff,se) 
    }
  }
  rz$year<-yr
  return(rz)
}
# 
rzlt19<-list()
rzlt20<-list()
rzlt21<-list()
rzlt22<-list()
for (i in 1:11) rzlt19[[i]]<-mdlr(i,2019) # warnings here are OK
for (i in 1:11) rzlt20[[i]]<-mdlr(i,2020)
for (i in 1:11) rzlt21[[i]]<-mdlr(i,2021)
for (i in 1:11) rzlt22[[i]]<-mdlr(i,2022)
rzlts<-rbind(do.call(rbind,rzlt19),do.call(rbind,rzlt20),
             do.call(rbind,rzlt21),do.call(rbind,rzlt22))
rzlts<-select(rzlts,region,year,y,x,coeff,se)

# hour-weighted IX
wix<-function(z,yr) {
  dta<-filter(opp[[z]],year==yr)  
  hta<-data.frame(utc_hour=c(0:23),solar=aggregate(solar~utc_hour,data=dta,mean)$solar,wind=aggregate(wind~utc_hour,data=dta,mean)$wind)
  gta<-list()
  for (i in 1:24) gta[[i]]<-feols(gas~wind+solar+load+ix|my,data=filter(dta,utc_hour==(i-1)),se='hetero')
  for (i in 1:24) hta$gas_coeff[i]<-gta[[i]]$coefficients[4]
  for (i in 1:24) hta$gas_se[i]<-gta[[i]]$se[4]
  for (i in 1:24) hta$gas_nwse[i]<-sqrt(diag(NeweyWest(gta[[i]])))[4]
  if (rgnz$region[z]!="NE") {
    cta<-list()
    for (i in 1:24) cta[[i]]<-feols(coal~wind+solar+load+ix|my,data=filter(dta,utc_hour==(i-1)),se='hetero')
    for (i in 1:24) hta$coal_coeff[i]<-cta[[i]]$coefficients[4]
    for (i in 1:24) hta$coal_se[i]<-cta[[i]]$se[4]
    for (i in 1:24) hta$coal_nwse[i]<-sqrt(diag(NeweyWest(cta[[i]])))[4]
    dla<-data.frame(region=rgnz$region[z],year=yr,
                    y=c("int_coal","int_coal","int_gas","int_gas"),
                    x=c("ix_solar","ix_wind","ix_solar","ix_wind"),
                    coeff=c(weighted.mean(hta$coal_coeff,hta$solar),weighted.mean(hta$coal_coeff,hta$wind),
                            weighted.mean(hta$gas_coeff,hta$solar),weighted.mean(hta$gas_coeff,hta$wind)),
                    se=c(weighted.mean(hta$coal_nwse,hta$solar),weighted.mean(hta$coal_nwse,hta$wind),
                         weighted.mean(hta$gas_nwse,hta$solar),weighted.mean(hta$gas_nwse,hta$wind)))
  } else {
    dla<-data.frame(region=rgnz$region[z],year=yr,
                    y=c("int_gas","int_gas"),
                    x=c("ix_solar","ix_wind"),
                    coeff=c(weighted.mean(hta$gas_coeff,hta$solar),weighted.mean(hta$gas_coeff,hta$wind)),
                    se=c(weighted.mean(hta$gas_nwse,hta$solar),weighted.mean(hta$gas_nwse,hta$wind)))
  }
  return(dla)
}

wix19<-list()
wix20<-list()
wix21<-list()
wix22<-list()
for (i in 1:9) wix19[[i]]<-wix(i,2019)
for (i in 1:9) wix20[[i]]<-wix(i,2020)
for (i in 1:9) wix21[[i]]<-wix(i,2021)
for (i in 1:9) wix22[[i]]<-wix(i,2022)
wixa<-rbind(do.call(rbind,wix19),do.call(rbind,wix20),
            do.call(rbind,wix21),do.call(rbind,wix22))
xpt<-rbind(rzlts,wixa)
write.csv(xpt,"Outputs/gen_coefficients.csv",row.names=F)





