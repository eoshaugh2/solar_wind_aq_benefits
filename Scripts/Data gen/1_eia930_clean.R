
library(tidyverse)
library(lubridate)
library(readxl)

# USER INPUT: define these
WDopt<-"USER_INPUT" # WD for EIA output data
WDix<-"USER_INPUT" # WD for EIA IX data
WDisone<-"USER_INPUT" # WD for ISO-NE data
WDdta<-"USER_INPUT" # WD where data will be saved


# SET WORKING DIRECTORY
setwd(WDix)
nmCln<-function(z) as.numeric(gsub(",","",z)) # function to clean up the characters
ixa<-bind_rows(lapply(list.files(pattern="*.csv"),read.csv))
ixxa<-as.data.frame(ixa %>% mutate(w=abs(nmCln(Interchange..MW.))) %>% group_by(Region,DIBA_Region) %>%
                      summarize(w=sum(w,na.rm=T)))
nycw<-ixxa$w[ixxa$Region=="NY" & ixxa$DIBA_Region=="CAN"]
ixxp<-ixxa %>% filter(Region!=DIBA_Region,!(DIBA_Region%in%c("CAN","MEX"))) %>% inner_join(aggregate(w~Region,data=.,sum) %>% rename(W=w)) %>%
  mutate(W=ifelse(Region=="NY",W+nycw,W)) %>% mutate(wt=w/W) %>% select(Region,DIBA_Region,wt) 
write.csv(ixxp,paste(WDdta,"trading_regions.csv",sep=""),row.names=F)

# clean dates
dts<-matrix(unlist(str_split(ixa$UTC.Time.at.End.of.Hour," ")),ncol=3,byrow=T)
ixx<-ixa %>% mutate(local_date=as.Date(Data.Date,"%m/%d/%Y"),
                    utc_date=as.Date(dts[,1],"%m/%d/%Y"),utc_hour=as.numeric(matrix(unlist(str_split(dts[,2],":")),ncol=3,byrow=T)[,1]),utc_di=dts[,3]) %>%
  mutate(utc_hour=ifelse(utc_di=="PM" & utc_hour==12,12,ifelse(utc_di=="PM",utc_hour+12,ifelse(utc_di=="AM" & utc_hour==12,0,utc_hour)))) %>%
  select(-utc_di) %>% mutate(ix=nmCln(Interchange..MW.),year=year(utc_date))

# for regression purposes, drop within-region exchanges
ixy<-as.data.frame(ixx %>% filter(Region!=DIBA_Region) %>% group_by(Region,utc_date,utc_hour) %>%
                     summarize(ix=sum(ix,na.rm=T))) 

setwd(WDopt) # output
fls<-list.files(pattern="*.csv")
sts<-list()
for (i in 1:length(fls)) sts[[i]]<-read.csv(fls[i]) # 
sta<-do.call(rbind,sts)
# clean the hours
dts<-matrix(unlist(str_split(sta$UTC.Time.at.End.of.Hour," ")),ncol=3,byrow=T)
lts<-matrix(unlist(str_split(sta$Local.Time.at.End.of.Hour," ")),ncol=3,byrow=T)
sta<-sta %>% mutate(local_date=as.Date(Data.Date,"%m/%d/%Y"),
                    utc_date=as.Date(dts[,1],"%m/%d/%Y"),utc_hour=as.numeric(matrix(unlist(str_split(dts[,2],":")),ncol=3,byrow=T)[,1]),utc_di=dts[,3],
                    local_hour=as.numeric(matrix(unlist(str_split(lts[,2],":")),ncol=3,byrow=T)[,1]),local_di=lts[,3]) %>%
  mutate(year=year(utc_date),utc_hour=ifelse(utc_di=="PM" & utc_hour==12,12,ifelse(utc_di=="PM",utc_hour+12,ifelse(utc_di=="AM" & utc_hour==12,0,utc_hour))),
         local_hour=ifelse(local_di=="PM" & local_hour==12,12,ifelse(local_di=="PM",local_hour+12,ifelse(local_di=="AM" & local_hour==12,0,local_hour)))) %>%
  select(-utc_di,-local_di)

# now combine
dta<-sta %>% rename(region=Region) %>%
  mutate(gen=nmCln(Net.Generation..MW.),load=nmCln(Demand..MW.),load_imputed=nmCln(Demand..MW...Imputed.),coal=nmCln(Net.Generation..MW..from.Coal),
         ix=nmCln(Total.Interchange..MW.),gas=nmCln(Net.Generation..MW..from.Natural.Gas),solar=nmCln(Net.Generation..MW..from.Solar),
         hydro=nmCln(Net.Generation..MW..from.Hydropower.and.Pumped.Storage),wind=nmCln(Net.Generation..MW..from.Wind)) %>%
  mutate(load=ifelse(load<0 | is.na(load),load_imputed,load), # make a clean but comprehensive load var 
         solar=ifelse(solar<0,NA,solar),wind=ifelse(wind<0,NA,wind),gas=ifelse(gas<0,NA,gas),coal=ifelse(coal<0,NA,coal),
         overage=gen-load)  # drop negative gen (errors?)

# collapse to regional level, condensing three western regions into a single WECC
rta<-as.data.frame(dta %>% group_by(region,utc_date,utc_hour,year) %>%
                     summarize(load=sum(load,na.rm=T),coal=sum(coal,na.rm=T),gas=sum(gas,na.rm=T),
                               solar=sum(solar,na.rm=T),hydro=sum(hydro,na.rm=T),wind=sum(wind,na.rm=T),overage=sum(overage,na.rm=T),
                               local_date=min(local_date,na.rm=T),local_hour=min(local_hour,na.rm=T))) %>%
  inner_join(rename(ixy,region=Region))

# ISO-NE solar directly from ISO-NE ~ https://www.iso-ne.com/isoexpress/web/reports/operations/-/tree/daily-gen-fuel-type
nea<-as.data.frame(rbind(read_excel(paste(WDisone,"hourly_solar_gen_2018.xlsx",sep = ""), sheet = "HourlyData"),
                         read_excel(paste(WDisone,"hourly_solar_gen_2019.xlsx",sep = ""), sheet = "HourlyData"),
                         read_excel(paste(WDisone,"hourly_solar_gen_2020.xlsx",sep = ""), sheet = "HourlyData"),
                         read_excel(paste(WDisone,"hourly_solar_gen_2021.xlsx",sep = ""), sheet = "HourlyData"),
                         read_excel(paste(WDisone,"hourly_solar_gen_2022.xlsx",sep = ""), sheet = "HourlyData"))) %>%
  mutate(neSolar=ifelse(is.na(tot_solar_mwh),0,tot_solar_mwh),local_date=as.Date(local_day),
         local_hour=as.numeric(as.character(LOCAL_HOUR_END)))
rta<-left_join(rta,select(nea,local_date,local_hour,neSolar)) %>%
  mutate(solar=ifelse(region=="NE",neSolar,solar)) %>% select(-neSolar)

# interchange control var ~ create weighted and unweighted versions
iuts<-matrix(unlist(str_split(ixa$UTC.Time.at.End.of.Hour," ")),ncol=3,byrow=T)
ixa<-ixa %>% mutate(ixMW=nmCln(Interchange..MW.),
                    local_date=as.Date(Data.Date,"%m/%d/%Y"),
                    utc_date=as.Date(iuts[,1],"%m/%d/%Y"),utc_hour=as.numeric(matrix(unlist(str_split(iuts[,2],":")),ncol=3,byrow=T)[,1]),utc_di=iuts[,3]) %>%
  mutate(year=year(utc_date),netIX=abs(ixMW),utc_hour=ifelse(utc_di=="PM" & utc_hour==12,12,ifelse(utc_di=="PM",utc_hour+12,ifelse(utc_di=="AM" & utc_hour==12,0,utc_hour)))) %>% 
  select(-utc_di)
ixx<-aggregate(netIX~Region+DIBA_Region+year,data=ixa,sum) %>%
  filter(Region!=DIBA_Region) %>%
  inner_join(aggregate(netIX~Region+year,data=.,sum) %>% rename(totalIX=netIX)) %>%
  inner_join(aggregate(load~region+year,data=rta,sum) %>% rename(Region=region)) %>%
  mutate(wt=(netIX/totalIX)*(totalIX/load)) %>% rename(region=Region)

# all BAs
rgs<-data.frame(id=c(1:13),region=sort(unique(rta$region)))
rgLst<-list()
for (i in 1:13) rgLst[[i]]<-sort(unique(ixx$DIBA_Region[ixx$region==rgs$region[i]]))

# create vars for load, solar, wind in each hour for each group of trading BAs
rgX<-list()
for (i in 1:13) rgX[[i]]<-as.data.frame(rta %>% filter(region%in%rgLst[[i]]) %>% 
                                          group_by(region,utc_date,year,utc_hour) %>% 
                                          summarize(load=sum(load,na.rm=T),solar=sum(solar,na.rm=T),wind=sum(wind,na.rm=T))) %>%
  rename(DIBA_Region=region) %>% mutate(region=rgs$region[i])
cntrl<-as.data.frame(do.call(rbind,rgX) %>% 
  inner_join(select(ixx,region,DIBA_Region,year,wt)) %>%
  group_by(region,utc_date,utc_hour) %>%
    summarize(exLoad=sum(load),exSolar=sum(solar),exWind=sum(wind)))
eza<-inner_join(rta,cntrl) 

# export that
xpt<-eza %>% select(region,utc_date,utc_hour,local_date,local_hour,load,ix,coal,gas,solar,hydro,wind,overage,
                    exLoad,exSolar,exWind) %>%
  filter(!is.na(load),!is.na(solar),!is.na(wind),solar>=0,wind>=0)

# collapse all the western regions into a single WECC
wecc<-as.data.frame(xpt %>% filter(region%in%c("CAL","NW","SW")) %>% group_by(utc_date,utc_hour) %>%
                      summarize(local_date=min(local_date),local_hour=min(local_hour),load=sum(load,na.rm=T),ix=sum(ix,na.rm=T),coal=sum(coal,na.rm=T),gas=sum(gas,na.rm=T),solar=sum(solar,na.rm=T),hydro=sum(hydro,na.rm=T),wind=sum(wind,na.rm=T),overage=sum(overage,na.rm=T),
                                exLoad=sum(exLoad,na.rm=T),exSolar=sum(exSolar,na.rm=T),exWind=sum(exWind,na.rm=T)))
xpt<-xpt %>% filter(!(region%in%c("CAL","NW","SW"))) %>% rbind(mutate(wecc,region="WECC"))

write.csv(xpt,paste(WDdta,"analysis_set.csv",sep=""),row.names=F)

        