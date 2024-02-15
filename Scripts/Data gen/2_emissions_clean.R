#

source("~/Desktop/Data/prep.R")

# define WD
setwd("USER_INPUT")
WDdta<-"USER_INPUT" # WD where output will be saved

eta <- read.csv("Data/egrid_plant_level.csv")
sort(unique(eta$SUBRGN))
intc<-data.frame(SUBRGN=sort(unique(eta$SUBRGN)),
                 intc=c(NA,NA,"W","W","T","E",NA,NA,rep('E',times=3),'W',rep('E',times=3),NA,
                        rep('E',times=3),'W',rep('E',times=7)))

xpt<-eta %>% inner_join(intc) %>% filter(!is.na(intc),PLFUELCT%in%c("COAL","GAS")) %>%
  select(PLFUELCT,PLNGENAN,intc,PLNOXRTA,PLSO2RTA,PLC2ERTA)
colnames(xpt)<-c("fuel","gen","intc","nox","so2","co2e")
xpt$gen<-gsub(",","",xpt$gen)
xpt$co2e<-gsub(",","",xpt$co2e)
xpt<-xpt %>% mutate(gen=as.numeric(as.character(gen)),nox=as.numeric(as.character(nox)),co2e=as.numeric(as.character(co2e)))
#write.csv(xpt,"~/Desktop/AQ/Data/interconnect_emissions_clean.csv",row.names=F)

# weighted mean and SE in each category by interconnect
xta<-filter(xpt,gen>0)
wtM<-function(x,ii,fl) {
  dta<-xta %>% mutate(v=x) %>% filter(intc==ii,fuel==fl,!is.na(v))
  return(weighted.mean(dta$v,dta$gen))
}
wtMc<-function(ii,fl) {
  vc<-c(NA,NA,NA)
  for (i in 1:3) vc[i]<-wtM(xta[,i+3],ii,fl)
  return(vc)
}

wtSe<-function(x,ii,fl) {
  dta<-xta %>% mutate(v=x) %>% filter(intc==ii,fuel==fl,!is.na(v)) 
  return((sqrt((sum(dta$gen*(dta$v-weighted.mean(dta$v,dta$gen))^2))/
           (((nrow(dta)-1)/nrow(dta))*sum(dta$gen))))/
           sqrt(nrow(dta)))
} 
wtSec<-function(ii,fl) {
  vc<-c(NA,NA,NA)
  for (i in 1:3) vc[i]<-wtSe(xta[,i+3],ii,fl)
  return(vc)
}
xpt<-data.frame(poll=rep(rep(names(xta[4:6]),times=3),times=2),intc=rep(rep(c("E","T","W"),each=3),times=2),y=rep(c("coal","gas"),each=9),
                wMean=c(wtMc("E","COAL"),wtMc("T","COAL"),wtMc("W","COAL"),wtMc("E","GAS"),wtMc("T","GAS"),wtMc("W","GAS")),
                wSe=c(wtSec("E","COAL"),wtSec("T","COAL"),wtSec("W","COAL"),wtSec("E","GAS"),wtSec("T","GAS"),wtSec("W","GAS"))) %>%
  mutate(wMean=wMean*0.453592,wSe=wSe*0.453592) %>% # converted to kg
  mutate(wMean=ifelse(poll=="co2e",wMean*10^-3,wMean),wSe=ifelse(poll=="co2e",wSe*10^-3,wSe)) # CO2 to tons
#write.csv(xpt,"~/Desktop/AQ/Outputs/weighted_emissions_factors.csv",row.names=F)

# wide
wta<-data.frame(intc=rep(c("E","T","W"),times=2),y=rep(c("coal","gas"),each=3),
                nox=xpt$wMean[xpt$poll=="nox"],noxSe=xpt$wSe[xpt$poll=="nox"],
                so2=xpt$wMean[xpt$poll=="so2"],so2Se=xpt$wSe[xpt$poll=="so2"],
                co2=xpt$wMean[xpt$poll=="co2e"],co2Se=xpt$wSe[xpt$poll=="co2e"])
setwd(WDdta)
write.csv(wta,"weighted_emissions_factors.csv",row.names=F)

