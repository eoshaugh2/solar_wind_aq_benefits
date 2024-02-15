# script to aggregate results up to national level

library(tidyverse)

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/")

# point estimates
gta<-read.csv("Outputs/point_estimates.csv") 
gta[,5:31]<-gta[,5:31]*-1 # flip everything
aeo<-read.csv("Data/wind_solar_shares.csv") %>%  rename(windWt=wind,solarWt=solar) %>%
  mutate(windWt=ifelse(wind_pct<0.01,0,windWt),solarWt=ifelse(solar_pct<0.01,0,solarWt))

hta<-as.data.frame(gta %>% inner_join(aeo) %>% group_by(year,y) %>%
                     summarize(wind=weighted.mean(wind,windWt),solar=weighted.mean(solar,solarWt),
                               dWindNox=weighted.mean(dWindNox,windWt),dWindSo2=weighted.mean(dWindSo2,windWt),dWindCo2=weighted.mean(dWindCo2,windWt),
                               dSolarNox=weighted.mean(dSolarNox,solarWt),dSolarSo2=weighted.mean(dSolarSo2,solarWt),dSolarCo2=weighted.mean(dSolarCo2,solarWt),
                               dWind=weighted.mean(dWind,windWt),dSolar=weighted.mean(dSolar,solarWt)))
write.csv(hta,"Outputs/national_point_estimates.csv",row.names=F)


# bootstrap
bta<-read.csv("Outputs/raw_bootstrap.csv") 
bxa<-as.data.frame(bta %>% group_by(region,year,iter) %>% # first sum up coal/gas
                     summarize(wind=sum(wind),solar=sum(solar),
                               dWindNox=sum(dWindNox),dWindSo2=sum(dWindSo2),dWindCo2=sum(dWindCo2),
                               dSolarNox=sum(dSolarNox),dSolarSo2=sum(dSolarSo2),dSolarCo2=sum(dSolarCo2))) %>% # collapse down from gas/coal level
  mutate(dWind=dWindNox+dWindSo2+dWindCo2,dSolar=dSolarNox+dSolarSo2+dSolarCo2,
         aqWind=dWindNox+dWindSo2,aqSolar=dSolarNox+dSolarSo2)

bxx<-as.data.frame(bxa %>% inner_join(aeo) %>%
                     group_by(iter,year) %>% # then get down to iteration level with weighted means
                     summarize(wind=weighted.mean(wind,windWt),solar=weighted.mean(solar,solarWt),
                               dWindNox=weighted.mean(dWindNox,windWt),dWindSo2=weighted.mean(dWindSo2,windWt),dWindCo2=weighted.mean(dWindCo2,windWt),
                               dSolarNox=weighted.mean(dSolarNox,solarWt),dSolarSo2=weighted.mean(dSolarSo2,solarWt),dSolarCo2=weighted.mean(dSolarCo2,solarWt),
                               dWind=weighted.mean(dWind,windWt),dSolar=weighted.mean(dSolar,solarWt),
                               aqWind=weighted.mean(aqWind,windWt),aqSolar=weighted.mean(aqSolar,solarWt)))

# last take q5/q95 across those iterations
q5<-function(z) quantile(z,.05,na.rm=T)
q95<-function(z) quantile(z,.95,na.rm=T)
bxz<-as.data.frame(bxx %>% group_by(year) %>%
                     summarize(wind_lo=q5(wind),wind_hi=q95(wind),solar_lo=q5(solar),solar_hi=q95(solar),
                               dWindNox_lo=q5(dWindNox),dWindNox_hi=q95(dWindNox),
                               dWindSo2_lo=q5(dWindSo2),dWindSo2_hi=q95(dWindSo2),
                               dWindCo2_lo=q5(dWindCo2),dWindCo2_hi=q95(dWindCo2),
                               dSolarNox_lo=q5(dSolarNox),dSolarNox_hi=q95(dSolarNox),
                               dSolarSo2_lo=q5(dSolarSo2),dSolarSo2_hi=q95(dSolarSo2),
                               dSolarCo2_lo=q5(dSolarCo2),dSolarCo2_hi=q95(dSolarCo2),
                               dWind_lo=q5(dWind),dWind_hi=q95(dWind),dSolar_lo=q5(dSolar),dSolar_hi=q95(dSolar),
                               aqWind_lo=q5(aqWind),aqWind_hi=q95(aqWind),aqSolar_lo=q5(aqSolar),aqSolar_hi=q95(aqSolar)))

# flip all values
bxz[,2:25]<-bxz[,2:25]*-1
write.csv(bxz,"Outputs/national_cis.csv",row.names=F)

# other percentiles for figure
# last take q5/q95 across those iterations
q25<-function(z) quantile(z,.25,na.rm=T)
q50<-function(z) quantile(z,.5,na.rm=T)
q75<-function(z) quantile(z,.75,na.rm=T)
bxp<-as.data.frame(bxx %>% 
                     group_by(year) %>%
                     summarize(dWind5=q5(dWind),dWind25=q25(dWind),dWind50=q50(dWind),dWind75=q75(dWind),dWind95=q95(dWind),
                               dSolar5=q5(dSolar),dSolar25=q25(dSolar),dSolar50=q50(dSolar),dSolar75=q75(dSolar),dSolar95=q95(dSolar),
                               aqWind5=q5(aqWind),aqWind25=q25(aqWind),aqWind50=q50(aqWind),aqWind75=q75(aqWind),aqWind95=q95(aqWind),
                               aqSolar5=q5(aqSolar),aqSolar25=q25(aqSolar),aqSolar50=q50(aqSolar),aqSolar75=q75(aqSolar),aqSolar95=q95(aqSolar)))

# flip all values
bxp[,2:21]<-bxp[,2:21]*-1
write.csv(bxp,"Outputs/national_qtls.csv",row.names=F)

