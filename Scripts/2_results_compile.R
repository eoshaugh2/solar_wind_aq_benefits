# script to compile regression results, generate bootstrap confidence intervals

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/") 
library(tidyverse)
library(sn)

rta<-read.csv("Outputs/gen_coefficients.csv")
rgnz<-data.frame(region=sort(unique(rta$region)),intc=c(rep("E",times=9),'T','W'))
aeo<-read.csv(paste("Data/wind_solar_shares.csv",sep="")) %>% filter(year==2022)
rta<-rta %>% mutate(coeff=ifelse(x%in%c("solar","ix_solar") & region%in%aeo$region[aeo$solar_pct<0.01] |
                                  x%in%c("wind","ix_wind") & region%in%aeo$region[aeo$wind_pct<0.01],0,coeff),
                    se=ifelse(x%in%c("solar","ix_solar") & region%in%aeo$region[aeo$solar_pct<0.01] |
                                x%in%c("wind","ix_wind") & region%in%aeo$region[aeo$wind_pct<0.01],0,se)) %>%
  mutate(coeff=ifelse(is.na(coeff),0,coeff),se=ifelse(is.na(se),0,se))
# to make all the calcs easier, make a balanced set
Nz<-c(length(unique(rta$region)),length(unique(rta$year)),length(unique(rta$y)),length(unique(rta$x)))
bla<-data.frame(region=rep(rgnz$region,each=Nz[2]*Nz[3]*Nz[4]),
                year=rep(rep(c(2019:2022),each=Nz[3]*Nz[4]),times=Nz[1]),
                y=rep(rep(unique(rta$y),each=Nz[4]),times=Nz[1]*Nz[2]),
                x=rep(unique(rta$x),times=Nz[1]*Nz[2]*Nz[3]),m=1) %>%
  full_join(rta) %>% select(-m) %>%
  mutate(coeff=ifelse(is.na(coeff),0,coeff),se=ifelse(is.na(se),0,se))
bla$coeff[bla$y=="int_ix"]<-bla$coeff[bla$y=="int_ix"]*-1 # for correct effect
bla$coeff[bla$y=="int_hydro"]<-bla$coeff[bla$y=="int_hydro"]*-1 # for correct effect


# reshape
rshp<-function(z) {
  dda<-bla %>% filter(y==z)
  rda<-dda %>% select(region,year,y) %>% unique(.) %>%
    left_join(filter(dda,x=="wind") %>% rename(wind=coeff,windSe=se) %>% select(-x)) %>%
    left_join(filter(dda,x=="solar") %>% rename(solar=coeff,solarSe=se) %>% select(-x)) %>%
    left_join(filter(dda,x=="hydro") %>% rename(hydro=coeff,hydroSe=se) %>% select(-x)) %>%
    left_join(filter(dda,x=="ix") %>% rename(ix=coeff,ixSe=se) %>% select(-x)) %>%
    left_join(filter(dda,x=="ix_wind") %>% rename(ixWind=coeff,ixWindSe=se) %>% select(-x)) %>%
    left_join(filter(dda,x=="ix_solar") %>% rename(ixSolar=coeff,ixSolarSe=se) %>% select(-x))
  rda[is.na(rda)]<-0
  return(rda)
} 

rsa<-list()
yl<-unique(bla$y)
for (i in 1:7) rsa[[i]]<-rshp(yl[i]) 
unique(bla$y) # 1 and 2 are all set, 3 needs to be worked into bot
gsa<-rsa[[1]] %>% select(-ix,-ixSe,-ixSolar,-ixSolarSe,-ixWind,-ixWindSe)
cla<-rsa[[2]] %>% select(-ix,-ixSe,-ixSolar,-ixSolarSe,-ixWind,-ixWindSe)
hyta<-rsa[[3]] %>% rename(wind_hydro=wind,wind_hydroSe=windSe,
                          solar_hydro=solar,solar_hydroSe=solarSe) %>%
  select(region,year,wind_hydro,wind_hydroSe,solar_hydro,solar_hydroSe)
ixGas<-rsa[[4]] %>% select(region,year,y,ixWind,ixWindSe,ixSolar,ixSolarSe,hydro,hydroSe) %>% mutate(y="gas") %>% rename(xHydro=hydro,xHydroSe=hydroSe)
ixCl<-rsa[[5]] %>% select(region,year,y,ixWind,ixWindSe,ixSolar,ixSolarSe,hydro,hydroSe) %>% mutate(y="coal")  %>% rename(xHydro=hydro,xHydroSe=hydroSe)
ixx<-rsa[[6]] %>% select(region,year,wind,windSe,solar,solarSe) %>% rename(xWind=wind,xWindSe=windSe,xSolar=solar,xSolarSe=solarSe)
ihy<-rsa[[7]] %>% select(region,year,ix,ixSe) %>% rename(hyIx=ix,hyIxSe=ixSe)
cmpl<-function(z1,z2) z1 %>% left_join(hyta) %>% left_join(z2) %>% left_join(ixx) %>% left_join(ihy)

gss<-cmpl(gsa,ixGas)
cll<-cmpl(cla,ixCl)

# weighted emissions factors and damages
weta <- read.csv("Data/weighted_emissions_factors.csv")
wdmg<-read.csv("Data/weighted_damage_factors.csv") %>%
  inner_join(data.frame(source=c("Coal","Natural Gas"),y=c("coal","gas"))) %>% select(-source) 
colnames(wdmg)<-c("intc",paste("d",names(wdmg)[2:7],sep=""),"y")

# this has all the sources of variation
pta<-rbind(gss,cll) %>% inner_join(rgnz) %>% inner_join(weta) %>% inner_join(wdmg)

# replicate this exact equation throughout
gta<-pta %>%
  mutate(inrWind=wind-(hydro*wind_hydro),ixWind=ixWind*xWind+xHydro*hyIx*xWind,  ### GENERATION
         inrSolar=solar-(hydro*solar_hydro),ixSolar=ixSolar*xSolar+xHydro*hyIx*xSolar) %>%
  mutate(wind=inrWind+ixWind,solar=inrSolar+ixSolar) %>% 
  mutate(dWindNox=wind*nox*dnox,dWindSo2=wind*so2*dso2,dWindCo2=wind*co2*dco2,
         dSolarNox=solar*nox*dnox,dSolarSo2=solar*so2*dso2,dSolarCo2=solar*co2*dco2) %>%
  mutate(dWind=dWindNox+dWindSo2+dWindCo2,dSolar=dSolarNox+dSolarSo2+dSolarCo2) 
xpt<-select(gta,region,intc,year,y,inrWind,ixWind,wind,inrSolar,ixSolar,solar,
            hydro,wind_hydro,solar_hydro,xHydro,xWind,xSolar,hyIx,
            nox,so2,co2,dnox,dso2,dco2,names(gta)[41:48])
 
# export those point estimates 
write.csv(xpt,"Outputs/point_estimates.csv",row.names=F)

# organize into a set you can iterate on
bst<-pta %>% select(names(pta)[c(4:25,27:38)],region,year,y)
names(bst) # check this, should be a coeff and and SE repeated until region, year, y

### for CO2
co2params<-cp2dp(c(bst$dco2[1],bst$dco2sd[1],0.995),"SN") # converted in skewed-normal parameters
qsn(0.05,xi=co2params[1], omega=co2params[2], alpha=co2params[3]) # gets the approximate distribution
qsn(0.95,xi=co2params[1], omega=co2params[2], alpha=co2params[3])


# 17 variables
bstrpr<-function(z) {
  a<-matrix(nrow=10000,ncol=16)
  sq<-seq(1,31,2) # everything up to CO2 is normal
  for (j in 1:16) a[,j]<-rnorm(10000,bst[z,sq[j]],bst[z,sq[j]+1])
  b<-as.data.frame(a)
  colnames(b)<-names(bst[sq])
  c<-b %>% mutate(iter=c(1:10000),region=bst$region[z],year=bst$year[z],y=bst$y[z])
  return(c)
}
boots<-list()
for (i in 1:nrow(pta)) boots[[i]]<-bstrpr(i)
straps<-do.call(rbind,boots)
co2strap<-data.frame(iter=c(1:10000),dco2=rsn(10000,co2params[1],co2params[2],co2params[3]))
c(mean(co2strap$dco2),quantile(co2strap$dco2,0.05),quantile(co2strap$dco2,0.95)) # should be close
straps<-inner_join(straps,co2strap)

bta<-straps %>%
  mutate(inrWind=wind-(hydro*wind_hydro),ixWind=ixWind*xWind+xHydro*hyIx*xWind,  ### GENERATION
         inrSolar=solar-(hydro*solar_hydro),ixSolar=ixSolar*xSolar+xHydro*hyIx*xSolar) %>%
  mutate(wind=inrWind+ixWind,solar=inrSolar+ixSolar) %>% 
  mutate(dWindNox=wind*nox*dnox,dWindSo2=wind*so2*dso2,dWindCo2=wind*co2*dco2,
         dSolarNox=solar*nox*dnox,dSolarSo2=solar*so2*dso2,dSolarCo2=solar*co2*dco2) %>%
  mutate(dWind=dWindNox+dWindSo2+dWindCo2,dSolar=dSolarNox+dSolarSo2+dSolarCo2) 
write.csv(bta,"Outputs/raw_bootstrap.csv",row.names=F) # save raw output for national estimate

# now estimate totals and get q5/q95 by region/year
q5<-function(z) quantile(z,.05,na.rm=T)
q95<-function(z) quantile(z,.95,na.rm=T)
bxa<-as.data.frame(bta %>% group_by(region,year,iter) %>%
                     summarize(wind=sum(wind),solar=sum(solar),
                               dWindNox=sum(dWindNox),dWindSo2=sum(dWindSo2),dWindCo2=sum(dWindCo2),
                               dSolarNox=sum(dSolarNox),dSolarSo2=sum(dSolarSo2),dSolarCo2=sum(dSolarCo2))) # collapse down from gas/coal level
bxx<-as.data.frame(bxa %>% group_by(region,year) %>%
                     summarize(wind_lo=q5(wind),wind_hi=q95(wind),solar_lo=q5(solar),solar_hi=q95(solar),
                               dWindNox_lo=q5(dWindNox),dWindNox_hi=q95(dWindNox),
                               dWindSo2_lo=q5(dWindSo2),dWindSo2_hi=q95(dWindSo2),
                               dWindCo2_lo=q5(dWindCo2),dWindCo2_hi=q95(dWindCo2),
                               dSolarNox_lo=q5(dSolarNox),dSolarNox_hi=q95(dSolarNox),
                               dSolarSo2_lo=q5(dSolarSo2),dSolarSo2_hi=q95(dSolarSo2),
                               dSolarCo2_lo=q5(dSolarCo2),dSolarCo2_hi=q95(dSolarCo2))) %>%
  mutate(dWind_lo=dWindNox_lo+dWindSo2_lo+dWindCo2_lo,dWind_hi=dWindNox_hi+dWindSo2_hi+dWindCo2_hi,
         dSolar_lo=dSolarNox_lo+dSolarSo2_lo+dSolarCo2_lo,dSolar_hi=dSolarNox_hi+dSolarSo2_hi+dSolarCo2_hi)
write.csv(bxx,"Outputs/damages_bstrap_cis.csv",row.names=F)

