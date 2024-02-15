
library(tidyverse)
library(utils)

# define these
WDont<-"USER_INPUT" # WD for all Ontario data ~ should include folders for Gen, Load, and ix
WDeia<-"USER_INPUT" # WD for EIA output
WDdta<-"USER_INPUT" # WD where output will be saved

### GEN
# http://reports.ieso.ca/public/GenOutputCapabilityMonth/PUB_GenOutputCapabilityMonth_202205.csv
# 2020 first year with full data, use that
pll<-data.frame(year=rep(c(2020:2022),each=12),month=rep(str_pad(c(1:12),pad='0',2),times=3)) %>%
  mutate(strng=paste(year,month,sep=""))
setwd(paste(WDont,"Gen",sep=""))

fls<-list()
for (i in 1:nrow(pll)) fls[[i]]<-read.csv(paste("ieso",pll$strng[i],".csv",sep=""), header=F) %>% mutate(x=c(1:nrow(.))) %>% filter(x>4)
fta<-do.call(rbind,fls)

nms <- read.csv(paste("ieso",pll$strng[1],".csv",sep=""), header=F) %>% mutate(x=c(1:nrow(.))) %>% filter(x==4)
colnames(fta)<-as.vector(nms)
fta<-fta %>% filter(Measurement=="Output") %>% select(names(fta)[1:28]) %>% select(-Measurement)

lta<-as.data.frame(fta %>% pivot_longer(!c(`Delivery Date`,Generator,`Fuel Type`),names_to="local_hour",values_to="output")) %>%
  filter(`Fuel Type`%in%c("GAS","SOLAR","WIND","HYDRO"))
hrs<-str_split(lta$local_hour," ")
mtt<-matrix(unlist(hrs),ncol=2,byrow=T)
lta$local_hour<-as.numeric(mtt[,2])-1
gta<-as.data.frame(lta %>% rename(local_date=`Delivery Date`) %>% mutate(gas=as.numeric(output)) %>%
                     group_by(local_date,local_hour,`Fuel Type`) %>%
                     summarize(output=sum(gas,na.rm=T)))
gna<-full_join(filter(gta,`Fuel Type`=="GAS") %>% rename(gas=output) %>% select(-`Fuel Type`),
               full_join(filter(gta,`Fuel Type`=="WIND") %>% rename(wind=output) %>% select(-`Fuel Type`),
                         filter(gta,`Fuel Type`=="SOLAR") %>% rename(solar=output) %>% select(-`Fuel Type`))) %>%
  full_join(filter(gta,`Fuel Type`=="HYDRO") %>% rename(hydro=output) %>% select(-`Fuel Type`))
write.csv(gna,"ieso_gen_clean.csv",row.names=F)


#### LOAD
setwd(WDont)
lda<-rbind(read.csv("Load/PUB_Demand_2020.csv") %>% mutate(x=c(1:nrow(.))) %>% filter(x>3),
           read.csv("Load/PUB_Demand_2021.csv") %>% mutate(x=c(1:nrow(.))) %>% filter(x>3),
           read.csv("Load/PUB_Demand_2022.csv") %>% mutate(x=c(1:nrow(.))) %>% filter(x>3)) 
colnames(lda)<-c("local_date","local_hour","market_demand","ontario_demand","x")
lda<-lda %>% mutate(local_hour=as.numeric(local_hour)-1) %>% select(-x)

ixa<-rbind(read.csv("ix/PUB_IntertieScheduleFlowYear_2020.csv") %>% mutate(x=c(1:nrow(.))) %>% filter(x>4),
           read.csv("ix/PUB_IntertieScheduleFlowYear_2021.csv") %>% mutate(x=c(1:nrow(.))) %>% filter(x>4),
           read.csv("ix/PUB_IntertieScheduleFlowYear_2022.csv") %>% mutate(x=c(1:nrow(.))) %>% filter(x>4)) %>%
  mutate(ix=as.numeric(X.44)-as.numeric(X.43)) %>% # position 43 is imports, 44 is exports, make net exports consistent with EIA
  select(names(.)[1:2],ix) 
colnames(ixa)<-c("local_date","local_hour","ix")
ixa$local_hour<-as.numeric(ixa$local_hour)-1

# all and pull in UTC hour
cmpl<-full_join(gna,full_join(lda,ixa)) %>% unique(.) %>% 
  mutate(local_date=as.Date(local_date),load=as.numeric(ontario_demand),ix=as.numeric(ix)) %>%
  select(-ontario_demand,-market_demand)
# just replicate 2020 for 2019 values
bnd<-as.data.frame(cmpl %>% mutate(mnth=month(local_date),day=day(local_date)) %>% group_by(mnth,day,local_hour) %>%
                     summarize(gas=mean(gas),wind=mean(wind),solar=mean(solar),load=mean(load),hydro=mean(hydro),ix=mean(ix))) %>%
  mutate(local_date=paste("2019-",str_pad(mnth,pad="0",2),"-",str_pad(day,pad="0",2),sep="")) %>%
  mutate(local_date=as.Date(local_date)) %>% select(-mnth,-day)
cmpl<-rbind(bnd,cmpl)

# pull in UTC hour based on NY local time
setwd(WDeia) # output
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
  mutate(utc_hour=ifelse(utc_di=="PM" & utc_hour==12,12,ifelse(utc_di=="PM",utc_hour+12,ifelse(utc_di=="AM" & utc_hour==12,0,utc_hour))),
         local_hour=ifelse(local_di=="PM" & local_hour==12,12,ifelse(local_di=="PM",local_hour+12,ifelse(local_di=="AM" & local_hour==12,0,local_hour)))) %>%
  select(-utc_di,-local_di)

nya<-filter(sta,Region=="NY") %>% select(local_date,local_hour,utc_date,utc_hour) %>% unique(.)
xta<-left_join(cmpl,nya) %>% unique(.)
write.csv(xta,paste(WDdta,"ieso_clean.csv",sep=''),row.names=F)

