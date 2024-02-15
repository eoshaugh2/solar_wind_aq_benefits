# script to generate Figure 2

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/")
source("Scripts/prep.R")
aeo<-read.csv("Data/wind_solar_shares.csv") %>% filter(year==2022)
pta<-read.csv("Outputs/national_point_estimates.csv") %>% filter(year==2022) 
bta<-read.csv("Outputs/national_cis.csv") %>% filter(year==2022) 

# define the ± bounds based on mean difference between the two
bndr<-function() data.frame(x=c("solar","wind"),dmg=c(sum(pta$dSolar),sum(pta$dWind)),
                            lo=c(bta$dSolar_lo,bta$dWind_lo),hi=c(bta$dSolar_hi,bta$dWind_hi)) 
jta<-bndr()

# pctls
qta<-read.csv("Outputs/national_qtls.csv") %>% filter(year==2022)

hta<-read.csv("Outputs/point_estimates.csv") %>% filter(year==2022) %>%
  select(-wind,-solar) %>% inner_join(aeo) %>%
  mutate(tWindNox=dWindNox*wind,tWindSo2=dWindSo2*wind,tWindCo2=dWindCo2*wind,tWind=dWind*wind,
         tSolarNox=dSolarNox*solar,tSolarSo2=dSolarSo2*solar,tSolarCo2=dSolarCo2*solar,tSolar=dSolar*solar)
hta[,35:42]<-hta[,35:42]*-1 # flip everything

# three data sets come together, in each set you need long data with y=total damages
# total
ttl<-function() {
  as.data.frame(pta %>% group_by(year) %>% 
                       summarize(dWindNox=sum(dWindNox),dWindSo2=sum(dWindSo2),dWindCo2=sum(dWindCo2),
                                 dSolarNox=sum(dSolarNox),dSolarSo2=sum(dSolarSo2),dSolarCo2=sum(dSolarCo2),
                                 dWind=sum(dWind),dSolar=sum(dSolar))) %>% mutate(sw=sum(aeo$solar),ww=sum(aeo$wind)) %>%
    mutate(dSolar_lo=bta$dSolar_lo,dSolar_hi=bta$dSolar_hi,dWind_lo=bta$dWind_lo,dWind_hi=bta$dWind_hi) %>%
    mutate(tSolar=dSolar*sw,tSolar_lo=dSolar_lo*sw,tSolar_hi=dSolar_hi*sw,
           tWind=dWind*ww,tWind_lo=dWind_lo*ww,tWind_hi=dWind_hi*ww)
}
tta<-ttl()
(sum(hta$tSolar)-tta$tSolar)<1 # these should both be TRUE, means that these match the regional sums
(sum(hta$tWind)-tta$tWind)<1 # allowing for some rounding error

# get all the total figures in one place
totz<-data.frame(solar=c(round(tta$tSolar*10^-9),round(jta$dmg[jta$x=="solar"])),
                 wind=c(round(tta$tWind*10^-9),round(jta$dmg[jta$x=="wind"])))


# to long
lngr<-function() data.frame(x=c("solar","wind"),y=c(tta$tSolar,tta$tWind),tlo=c(tta$tSolar_lo,tta$tWind_lo),thi=c(tta$tSolar_hi,tta$tWind_hi)) 
lta<-lngr()
# other quantiles
qCln<-function(m,M) {
  data.frame(y=as.numeric(qta[,m:M]),x=rep(c("Wind","Solar"),each=5),
             lb1=rep(c("p95","p75","p50","p25","p5"),times=2)) %>% 
    mutate(yy=ifelse(x=="Solar",y*sum(aeo$solar),y*sum(aeo$wind))*10^-9) %>%
    mutate(yyy=yy,lb2=paste("$",round(yy),"B ($",round(y),"/MWh)",sep=""))
}
qqa<-qCln(2,11)

# by source/pollutant
pla<-pta %>% mutate(sw=sum(aeo$solar),ww=sum(aeo$wind)) %>%
  mutate(tSolarNox=dSolarNox*sw,tSolarSo2=dSolarSo2*sw,tSolarCo2=dSolarCo2*sw,
         tWindNox=dWindNox*ww,tWindSo2=dWindSo2*ww,tWindCo2=dWindCo2*ww)
pst<-function(z) rep(paste(z,c("a","b"),sep=""),times=2)
lcln<-function(z,vv) data.frame(x=z,y=vv,
           f=rep(pla$y,times=3),fl=c("a","d","b","e","c","f"))
pya<-rbind(lcln("solar",c(pla$tSolarNox,pla$tSolarSo2,pla$tSolarCo2)),
           lcln("wind",c(pla$tWindNox,pla$tWindSo2,pla$tWindCo2)))
# by region
rgnl<-function() {
  a<-as.data.frame(hta %>% group_by(region) %>% summarize(tSolar=sum(tSolar),tWind=sum(tWind)))
  # make long
  b<-data.frame(region=rep(a$region,times=2),x=rep(c("solar","wind"),each=nrow(a)),ttl=c(a$tSolar,a$tWind))
  c<-as.data.frame(b %>%  # collapse down to key regions
                       inner_join(data.frame(region=rep(sort(unique(aeo$region)),times=2),x=rep(c("solar","wind"),each=length(unique(aeo$region))),
                                             cRegion=c("CAR","1Other","FLA","1Other","1Other","1Other","1Other","SE","1Other","TEX","WECC",
                                                       "1Other","CENT","1Other","1Other","MIDW",rep("1Other",times=4),"TEX","WECC"))) %>%
                       group_by(cRegion,x) %>% summarize(y=sum(ttl))) %>%
    inner_join(data.frame(cRegion=sort(unique(.$cRegion)),
                          fll=c("gray50","darkcyan","mediumpurple4","dodgerblue","lightsteelblue3","khaki4","mediumpurple","midnightblue")))
  return(c)
}
rya<-rgnl()

# create two alt sets, solar and wind
nmCln<-data.frame(cRegion=sort(unique(rya$cRegion)),
                  lb=c("Other","Carolinas","Central U.S.","Florida","Midwest","Southeast","Texas","Western U.S."))
fclnr<-function(z) {
  rbind(lta %>% filter(x==z) %>% mutate(pos=1,fl="z",f="coal",cRegion=NA) %>% select(pos,y,fl,f,tlo,thi,cRegion),
        pya %>% filter(x==z) %>% mutate(pos=2,tlo=NA,thi=NA,cRegion=NA) %>% select(pos,y,fl,f,tlo,thi,cRegion),
        rya %>% filter(x==z) %>% mutate(pos=3,tlo=NA,thi=NA,f="coal",fl=cRegion) %>% select(pos,y,fl,f,tlo,thi,cRegion)) %>%
    arrange(cRegion) %>% mutate(lx=c(1:nrow(.))) %>% arrange(-lx) %>% mutate(labY=ifelse(is.na(cRegion),0,y)) %>%
    mutate(labPos=ave(labY,FUN=cumsum)) %>% mutate(lp=lag(labPos),ld=c(NA,diff(labPos))) %>%
    mutate(labPos=lp+0.5*ld) %>% left_join(nmCln)
}
sla<-fclnr("solar") %>% mutate(lb=ifelse(lb=="Other",NA,lb))
wna<-fclnr("wind")
slv<-sort(unique(sla$fl)) %>% print(.)
wnv<-sort(unique(wna$fl)) %>% print(.)

# slip "Florida" out, it doesn't fit
sla$lb[sla$lb=="Florida"]<-NA

tfgr<-function(d,ttl,yM,y1,y2,y3,yG,yC,yNox,ySo,yCo) {
  if (ttl=="Solar") grd<-c("gray50","firebrick4","bisque4","cornflowerblue",unique(rya$fll[rya$cRegion%in%slv[5]]),"firebrick4","bisque4","cornflowerblue",unique(rya$fll[rya$cRegion%in%slv[9:13]]),"slategrey")
  else grd<-c("gray50","firebrick4","bisque4","cornflowerblue",unique(rya$fll[rya$cRegion%in%wnv[5]]),"firebrick4","bisque4","cornflowerblue",unique(rya$fll[rya$cRegion%in%wnv[9:11]]),"slategrey")
  a<-fgBase(d,ttl,yM,y1,y2,y3,yG,yC,yNox,ySo,yCo,grd,'firebrick4')
  return(a)
}
ggsave("Figures/damage_stacks_solar.jpg",
       plot=tfgr(sla,"Solar",22,6.8,6.96,11.5,3.5,8.6,12.2,13.9,8.6)+
                        ant(bquote(SO[2]),2,10.6,3,'white')+
         annotate("text",x=0.85,y=12.7,label="Point estimates:",size=2.7,color='slategrey',hjust=0)+
         annotate("text",x=0.85,y=12,label=paste("$",totz$solar[1],"B ($",totz$solar[2],"/MWh)",sep=""),size=2.7,color='slategrey',hjust=0)+
         ant("Florida",2.56,10,2.2,'dodgerblue'),width=6,height=4)
ggsave("Figures/damage_stacks_wind.jpg",
       plot=tfgr(wna,"Wind",120,24.5,25,62,12,44,65,54,38)+
         annotate("text",x=0.85,y=68.9,label="Point estimates:",size=2.7,color='slategrey',hjust=0)+
         annotate("text",x=0.85,y=64.6,label=paste("$",totz$wind[1],"B ($",totz$wind[2],"/MWh)",sep=""),size=2.7,color='slategrey',hjust=0),width=6,height=4)



