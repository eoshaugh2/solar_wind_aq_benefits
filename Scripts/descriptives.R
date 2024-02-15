# script to export descriptive results

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/")
source("Scripts/prep.R")

aeo<-read.csv("Data/wind_solar_shares.csv") 
pta<-read.csv("Outputs/national_point_estimates.csv") 
bna<-read.csv("Outputs/national_cis.csv") 
hta<-read.csv("Outputs/point_estimates.csv") 
bsa <- read.csv("Outputs/raw_bootstrap.csv")
hta[,c(5:17,24:31)]<-hta[,c(5:17,24:31)]*-1 # flip everything

# Table 1 MWh/MWh
p5<-function(z) -quantile(z,.05)
p95<-function(z) -quantile(z,.95)
t1<-as.data.frame(bsa %>% filter(year==2022) %>% group_by(region,year,y) %>%
                    summarize(solar5=p5(solar),solar95=p95(solar),
                              wind5=p5(wind),wind95=p95(wind))) %>%
  inner_join(hta %>% filter(year==2022) %>% select(year,y,region,solar,wind)) %>% 
  mutate(sr1=solar5-solar,sr2=solar-solar95,wr1=wind5-wind,wr2=wind-wind95) %>%
  mutate(solarRng=(sr1+sr2)/2,windRng=(wr1+wr2)/2) %>%
  mutate(solar_clean=paste(round(solar,2)," (±",round(solarRng,2),")",sep=""),
         wind_clean=paste(round(wind,2)," (±",round(windRng,2),")",sep=""))
# national calculated from weighted mean across iterations in bootstraps
nBsa<-as.data.frame(filter(bsa,year==2022) %>% 
                      inner_join(filter(aeo,year==2022) %>% select(region,solar,wind) %>% rename(windWt=wind,solarWt=solar)) %>%
                     group_by(iter,y) %>% # then get down to iteration level with weighted means
                     summarize(wind=weighted.mean(wind,windWt),solar=weighted.mean(solar,solarWt)))
t1Ntl<-data.frame(region="National",y=c("coal","gas"),
                 solar=aggregate(solar~y,data=filter(pta,year==2022),mean)$solar,
                 wind=aggregate(wind~y,data=filter(pta,year==2022),mean)$wind,
                 s5=aggregate(solar~y,data=nBsa,p5)$solar,s95=aggregate(solar~y,data=nBsa,p95)$solar,
                 w5=aggregate(wind~y,data=nBsa,p5)$wind,w95=aggregate(wind~y,data=nBsa,p95)$wind) %>%
  mutate(sr1=s5-solar,sr2=solar-s95,wr1=w5-wind,wr2=wind-w95) %>%
  mutate(solarRng=(sr1+sr2)/2,windRng=(wr1+wr2)/2) %>%
  mutate(solar_clean=paste(round(solar,2)," (±",round(solarRng,2),")",sep=""),
         wind_clean=paste(round(wind,2)," (±",round(windRng,2),")",sep=""))
tb1<-rbind(select(t1,region,y,solar_clean,wind_clean),select(t1Ntl,region,y,solar_clean,wind_clean))
tbl1<-data.frame(region=unique(tb1$region),wind_coal=tb1$wind_clean[tb1$y=="coal"],wind_gas=tb1$wind_clean[tb1$y=="gas"],
                 solar_coal=tb1$solar_clean[tb1$y=="coal"],solar_gas=tb1$solar_clean[tb1$y=="gas"]) %>%
  inner_join(filter(aeo,year==2022) %>% select(region,wind_pct,solar_pct) %>%
               rbind(data.frame(region="National",wind_pct=0.11,solar_pct=0.03))) %>%
  mutate(wind_pct=round(wind_pct,2),solar_pct=round(solar_pct,2)) %>%
  select(region,wind_pct,wind_coal,wind_gas,solar_pct,solar_coal,solar_gas)
write.csv(tbl1,"Outputs/mwh_mwh_outputs.csv",row.names=F)

# AEO
# sample defined by regions with >1% of resource in 2022
ata<-aeo %>% inner_join(filter(aeo,year==2022) %>% select(region,wind_pct,solar_pct) %>%
                          rename(w22=wind_pct,s22=solar_pct)) %>%
  mutate(solar=ifelse(s22<0.01,NA,solar),wind=ifelse(w22<0.01,NA,wind))
opt<-rbind(aggregate(wind~year,data=ata,sum,na.rm=T) %>% rename(aeo=wind) %>% mutate(x="wind"),
           aggregate(solar~year,data=ata,sum,na.rm=T) %>% rename(aeo=solar) %>% mutate(x="solar")) %>% filter(year<2023)

# avoided tons per pollutant and total avoided damages
dsc<-function(xx,yy,p1,p2,p3,zz) {
  a<-hta %>% inner_join(aeo %>% mutate(mwh=zz) %>% select(region,year,mwh)) %>% 
    mutate(vv=xx,dv=yy,pl1=p1,pl2=p2,pl3=p3)
  b<-a %>% mutate(avnox=vv*mwh*nox,avso2=vv*mwh*so2,avco2=vv*mwh*co2,
                  dnox=mwh*pl1,dso2=mwh*pl2,dco2=mwh*pl3,
                  tdmg=mwh*dv)
  c<-as.data.frame(b %>% group_by(year) %>%
                     summarize(avnox=sum(avnox),avso2=sum(avso2),avco2=sum(avco2),
                               dnox=sum(dnox),dso2=sum(dso2),dco2=sum(dco2),
                               tdmg=sum(tdmg)))
  return(c)
}
xpt<-rbind(dsc(hta$wind,hta$dWind,hta$dWindNox,hta$dWindSo2,hta$dWindCo2,aeo$wind) %>% mutate(x="wind"),
           dsc(hta$solar,hta$dSolar,hta$dSolarNox,hta$dSolarSo2,hta$dSolarCo2,aeo$solar) %>% mutate(x="solar")) %>%
  inner_join(opt)
xpt
write.csv(xpt,"~/Desktop/AQ/Data_scripts/Outputs/summary_damages.csv",row.names=F)

# combined point and boostrap estimates: national
as.data.frame(pta %>% group_by(year) %>% summarize(wind=sum(dWind),solar=sum(dSolar)))
select(bna,year,dSolar_hi,dSolar_lo,dWind_hi,dWind_hi,dWind_lo)

# combined point and boostrap estimates: regions
bta <- read.csv("~/Desktop/AQ/Data_Scripts/Outputs/damages_bstrap_cis.csv")
filter(bta,year==2022) %>% select(region,dWind_hi,dWind_lo)
filter(bta,year==2022) %>% select(region,dSolar_hi,dSolar_lo)

# MWh/MWh estimates
gta<-pta %>% select(year,y,wind,solar) %>% mutate(region="National") %>%
  rbind(select(hta,year,region,y,wind,solar)) %>% arrange(region,year)
lta<-inner_join(filter(gta,y=="coal") %>% rename(wind_coal=wind,solar_coal=solar) %>% select(-y),
                filter(gta,y=="gas") %>% rename(wind_gas=wind,solar_gas=solar) %>% select(-y))
xpt<-select(lta,region,year,wind_coal,wind_gas,solar_coal,solar_gas) %>% arrange(year,region)
write.csv(xpt,"~/Desktop/AQ/Data_Scripts/Outputs/mwh_mwh_ests.csv",row.names=F)

# damages with CIs
rga<-hta %>% filter(year==2022) %>% group_by(region) %>%
  summarize(solar=sum(dSolar),wind=sum(dWind))
bsta <- read.csv("~/Desktop/AQ/Outputs/damages_bstrap_cis.csv") %>% filter(year==2022) %>%
  mutate(windy=paste(-round(dWind_hi),-round(dWind_lo),sep="-"),
         sunny=paste(-round(dSolar_hi),-round(dSolar_lo),sep="-"))
rgg<-inner_join(rga,select(bsta,region,windy,sunny)) %>% mutate(solar=round(solar),wind=round(wind))
ntla<-pta %>% filter(year==2022) %>% summarize(solar=sum(dSolar),wind=sum(dWind)) %>%
  mutate(solar=round(solar),wind=round(wind))
ntlb<-bna %>% filter(year==2022) %>% mutate(windy=paste(round(dWind_hi),round(dWind_lo),sep="-"),
                                            sunny=paste(round(dSolar_hi),round(dSolar_lo),sep="-")) %>% select(sunny,windy)
rza<-rbind(data.frame(region="National",solar=ntla$solar,wind=ntla$wind,windy=ntlb$windy,sunny=ntlb$sunny),
           rgg) %>% select(region,wind,windy,solar,sunny)
colnames(rza)<-c("region","wind","wind_range","solar","solar_range")
write.csv("Outputs/damages_by_region.csv",row.names=F)

# for SI, table containing all regression coefficients and SEs
gta <- read.csv("~/Desktop/AQ/Data_Scripts/Outputs/gen_coefficients.csv")
# reshape to wide
gga<-gta %>% mutate(bd=ifelse(coeff<0,coeff+1.96*se,coeff-1.96*se)) %>%
  mutate(sig=ifelse(coeff<0 & bd<0,"*",ifelse(coeff>0 & bd>0,"*",""))) %>%
  mutate(result=paste("'",round(coeff,2),sig," (",round(se,2),")",sep="")) %>%
  select(region,year,y,x,result)

# Eq. 3, Y=coal,gas
# Eq. 4, Y=hydro
# Eq. 5, Y=ix
# Eq. 6, Y=ixcoal, ixgas, X=ix
# Eq. 7, Y=ixcoal, ixgas, X=ixhydro
# Eq. 8, Y=ixhydro

# Equations 3-5, in-region
ira<-filter(gga,y%in%c("coal","gas","hydro","int_ix"))
irx<-full_join(filter(ira,x=="wind") %>% rename(wind=result) %>% select(-x),
               filter(ira,x=="solar") %>% rename(solar=result) %>% select(-x)) %>%
  full_join(filter(ira,x=="hydro") %>% rename(hydro=result) %>% select(-x)) %>%
  mutate(wind=ifelse(is.na(wind),"",wind),solar=ifelse(is.na(solar),"",solar),hydro=ifelse(is.na(hydro),"",hydro)) %>%
  arrange(year,region,y) %>% mutate(y=ifelse(y=="int_ix","ix",y))
write.csv(irx,"Outputs/coeffs_eq3_5.csv",row.names=F)  

# Equations 6-8, trading regions
ora<-filter(gga,!(y%in%c("coal","gas","hydro","int_ix"))) %>%
  mutate(y=ifelse(y=="int_coal","coal",ifelse(y=="int_gas","gas","hydro")))
orx<-full_join(filter(ora,x=="ix_wind") %>% rename(ix_wind=result) %>% select(-x),
               filter(ora,x=="ix_solar") %>% rename(ix_solar=result) %>% select(-x)) %>%
  full_join(full_join(filter(ora,x=="hydro") %>% rename(hydro=result) %>% select(-x),
                      filter(ora,x=="ix") %>% rename(ix=result) %>% select(-x))) %>%
  mutate(ix_wind=ifelse(is.na(ix_wind),"",ix_wind),ix_solar=ifelse(is.na(ix_solar),"",ix_solar),
         ix=ifelse(is.na(ix),"",ix),hydro=ifelse(is.na(hydro),"",hydro)) %>%
  arrange(year,region,y)
write.csv(orx,"Outputs/coeffs_eq6_8.csv",row.names=F)  


  
  
