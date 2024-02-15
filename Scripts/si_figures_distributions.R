# script to generate certain figures from SI

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/")
source("Scripts/prep.R")
library(sn)
library(grid)
library(png)

###### REGRESSION STANDARD ERRORS
setwd("~/Desktop/AQ/Data_Scripts/")

gna <- read.csv("~/Desktop/AQ/Data_Scripts/Outputs/gen_coefficients.csv")
aeo <- read.csv("~/Desktop/AQ/Data_Scripts/Data/wind_solar_shares.csv")

gta<-gna %>% inner_join(select(aeo,region,year,wind_pct,solar_pct))

sta<-filter(gta,x=="solar",y!="int_hydro") %>% mutate(wt=solar_pct)
wta<-filter(gta,x=="wind",y!="int_hydro") %>% mutate(wt=wind_pct)

fgr<-function(dd,ttl) {
  base(dd)+geom_point(aes(wt*100,se,color=y,shape=y),alpha=0.8,size=5)+
    ggtitle(ttl)+xlab(paste(ttl,"Penetration (%)"))+
    scale_color_manual(values=c("black","darkcyan","dodgerblue","firebrick4"),labels=c("Coal","Gas","Hydro","IX"),name="Dependent\nVariable")+
    scale_shape_manual(values=c(16,17,18,19),labels=c("Coal","Gas","Hydro","IX"),name="Dependent\nVariable")+
    theme(legend.position=c(0.8,0.78))+ylab("SE")
}
ggsave("Figures/si_uncertainty_coefficient_correlation.jpg",
       plot=plot_grid(fgr(sta,"Solar")+
                        scale_y_continuous(breaks=c(0,1,2)),
                      fgr(wta,"Wind")+
                        scale_y_continuous(breaks=c(0,0.1,0.2),labels=c("0","0.1","0.2"))+
                        theme(axis.title.y=element_blank(),legend.position="none"),nrow=1),width=8,height=4)



###### DISTRIBUTIONS
weta <- read.csv("Data/weighted_emissions_factors.csv")
xx<-seq(0,3,0.001) # for creating distributions

### co2
cmplr<-function(z) {
  dd<-select(weta,names(weta)[c(z,z+1)])
  colnames(dd)<-c("mean","se")
  l<-list()
  for (i in 1:6) l[[i]]<-data.frame(x = xx, pdf = dnorm(xx,mean=dd$mean[i],sd=dd$se[i])) %>% mutate(f=i)
  d<-do.call(rbind,l) %>%
    inner_join(data.frame(f=c(1:6),region=rep(c("E","T","W"),times=2))) %>%
    mutate(typ=ifelse(f<4,"Coal","Gas"))
  return(d)
}
co2d<-cmplr(7)
fgr<-function(z,mxx) {
  a<-base(z)+geom_line(aes(x,pdf,color=region,linewidth=typ))+
    geom_point(data=mxx,aes(x,pdf,color=region,shape=typ),size=3)+
    ylab("Relative\nProbability")+
    scale_color_manual(values=c("tan1","navy","darkcyan"),guide="none")+
    scale_shape_manual(values=c("circle","diamond"),guide="none")+
    scale_linewidth_manual(values=c(1,0.5),guide="none")+theme(axis.text.y=element_blank())
  return(a)
}

# get mode to label the NOx figure
co2mx<-co2d %>% inner_join(aggregate(pdf~region+typ,data=co2d,max) %>% rename(mx=pdf)) %>%
  filter(pdf==mx)
ggsave("Figures/si_distributions_co2_emissions.jpg",
       plot=ggdraw()+draw_plot(fgr(co2d,co2mx)+
         coord_cartesian(xlim=c(0.05,1.2),ylim=c(2.98,60))+
         xlab(bquote(tCO[2]/MWh))+
    scale_x_continuous(breaks=seq(0,1.2,0.3),labels=c("0","0.3","0.6","0.9","1.2"))+
    ant("East",0.9,35,4.5,'tan1')+ant("Texas",1.12,20,4.5,'darkcyan')+ant("West",0.96,17,4.5,'navy'))+
      draw_grob(rasterGrob(readPNG('Figures/leg.png'),x=0.8,y=0.85,width=0.2)),width=6,height=4)


### NOx/SOx
nxsx<-rbind(cmplr(3) %>% mutate(poll="NOx"),
            cmplr(5) %>% mutate(poll="SO2"))
# get mode to label the NOx figure
nxsxmx<-nxsx %>% inner_join(aggregate(pdf~region+typ+poll,data=nxsx,max) %>% 
                           rename(mx=pdf)) %>% filter(pdf==mx)

ggsave("Figures/si_distributions_nox_sox_emissions.jpg",
       plot=ggdraw()+draw_plot(plot_grid(fgr(filter(nxsx,poll=="NOx"),filter(nxsxmx,poll=="NOx"))+
                        coord_cartesian(xlim=c(0,1),ylim=c(1.3,25))+
                        scale_x_continuous(breaks=seq(0,1,0.25),labels=c("0","0.25","0.5","0.75","1"))+
                        xlab("kg NOx/MWh")+
                        ant("East",0.58,17.5,4.5,'tan1')+ant("Texas",0.8,8.6,4.5,'darkcyan')+
                        ant("West",0.49,8,4.5,'navy'),
                      fgr(filter(nxsx,poll=="SO2"),filter(nxsxmx,poll=="SO2"))+
                        coord_cartesian(xlim=c(0,3),ylim=c(0.4,8))+
                        xlab(bquote("kg SO"[2]/MWh))+
                        theme(axis.title.y=element_text(color="white"))+
                        scale_x_continuous(breaks=c(0:3))+
                        ant("Gas*",0.2,7,5,'gray50'),nrow=2))+
         draw_grob(rasterGrob(readPNG('Figures/leg.png'),x=0.86,y=0.9,width=0.2)),width=6,height=6)


##### DAMAGES

# SCC
co2params<-cp2dp(c(185,115.55,0.995),"SN") # converted in skewed-normal parameters
qsn(0.05,xi=co2params[1], omega=co2params[2], alpha=co2params[3]) # gets the approximate distribution
qsn(0.95,xi=co2params[1], omega=co2params[2], alpha=co2params[3])
co2dx<-seq(0,600,0.001)
hant<-function(z,xx,yy) annotate('text',x=xx,y=yy,label=z,color='gray50',size=4,hjust=0)
rnrt<-data.frame(x=c(44,185,410),y=c(0.0015,0.0015,0.0015))
c2a<-data.frame(x = co2dx, pdf = dsn(x=co2dx,xi=co2params[1], omega=co2params[2], alpha=co2params[3])) #%>% mutate(f=i)
ggsave("Figures/si_distributions_co2_damages.jpg",
       plot=base(c2a)+geom_line(aes(x,pdf),color=lblblu)+
         geom_segment(data=rnrt,aes(x=x,xend=x,y=0,yend=y),color='gray50',linetype='dashed')+
         coord_cartesian(ylim=c(0.00018,0.004))+ylab("Relative\nProbability")+
         xlab(bquote("Social Cost of Carbon - $/tCO"[2]))+
         hant("Rennert et al.",47,0.0014)+hant("p5: $44",47,0.00115)+
         hant("Mean:",188,0.0014)+hant("$185",188,0.00115)+
         hant("p95: $410",413,0.0014)+
         theme(axis.text.y=element_blank()),width=6,height=4)

# NOx/SOx
wdmg<-read.csv("Data/weighted_damage_factors.csv") 
# collapse down to average (same for everything but NOx)
wta<-as.data.frame(wdmg %>% group_by(intc) %>%
                     summarize(nox=mean(nox),noxSd=mean(noxSd),so2=mean(so2),so2Sd=mean(so2Sd)))
cmplr<-function(z,zSd,xx) {
  l<-list()
  for (i in 1:6) l[[i]]<-data.frame(x = xx, pdf = dnorm(xx,mean=z[i],sd=zSd[i])) %>% mutate(f=i)
  d<-do.call(rbind,l) %>%
    inner_join(data.frame(f=c(1:3),region=c("E","T","W"))) 
  return(d)
}
nxsx<-rbind(cmplr(wta$nox,wta$noxSd,seq(0,100,0.1)) %>% mutate(poll="NOx"),
            cmplr(wta$so2,wta$so2Sd,seq(0,200,0.1)) %>% mutate(poll="SO2"))

noxSoxFgr<-function(pl,ym,yM) {
  a<-base(filter(nxsx,poll==pl))+geom_line(aes(x,pdf,color=region))+
    ylab("Relative\nProbability")+
    scale_color_manual(values=c("tan1","navy","darkcyan"),guide="none")+
    scale_alpha_manual(values=c(1,1),guide="none")+
    theme(plot.title=element_text(face='plain',size=16),axis.text.y=element_blank())
  return(a)
}

ggsave("Figures/si_distributions_nox_sox_damages.jpg",
       plot=plot_grid(noxSoxFgr("NOx",0,100)+ggtitle("NOx")+
                        coord_cartesian(xlim=c(0,120),ylim=c(0.0036,0.08))+noX+
                        xlab("NOx Damages - $/kg")+geom_point(x=0.2,y=12,color='gray50',shape='diamond',size=3)+
                        ant("East",65,0.058,4.5,'tan1')+ant("Texas",31,0.07,4.5,'darkcyan')+
                        ant("West",54,0.055,4.5,'navy'),
                      noxSoxFgr("SO2")+ggtitle(bquote(SO[2]))+
                        coord_cartesian(xlim=c(0,120),ylim=c(0.0026,0.063))+
                        xlab("Damages ($/kg)")+theme(axis.title.y=element_text(color="white"))+
                        scale_x_continuous(breaks=seq(0,100,25)),nrow=2,rel_heights=c(7,8)),width=6,height=6)


# figures to depict drivers of wind/solar results: regional vs. temporal
dta <- read.csv("Data/analysis_set.csv") %>%
  filter(region!="NY") %>% # exclude NY, no coal
  mutate(year=year(as.Date(local_date))) %>% filter(year==2022) %>%
  mutate(wind=ifelse(region%in%c("CAR","SE","FLA"),NA,wind),
         solar=ifelse(region%in%c("CENT","MIDW","MIDA"),NA,solar))


# by hour
hsta<-as.data.frame(dta %>% group_by(local_hour) %>% # exclude NY, no coal
                      summarize(coal=sum(coal),gas=sum(gas),load=sum(load),
                                solar=sum(solar,na.rm=T))) %>%
  mutate(cp=coal/load,gp=gas/load)
hwta<-as.data.frame(dta %>% group_by(local_hour) %>%
                      summarize(coal=sum(coal),gas=sum(gas),load=sum(load),
                                wind=sum(wind,na.rm=T))) %>%
  mutate(cp=coal/load,gp=gas/load)
hta<-data.frame(x=c(1,1,2,2),f=c('zCoal','aGas','zCoal','aGas'),
                y=c(weighted.mean(hsta$cp,hsta$solar),weighted.mean(hsta$gp,hsta$solar),
                    weighted.mean(hwta$cp,hwta$wind),weighted.mean(hwta$gp,hwta$wind))*100) %>%
  inner_join(aggregate(y~x,data=.,sum) %>% rename(tot=y)) %>%
  mutate(lab=paste(round(y),"%",sep=""),labY=ifelse(f=="zCoal",y+2,tot+2))

# by region
rsta<-as.data.frame(dta %>% group_by(region) %>%
                      summarize(coal=sum(coal),gas=sum(gas),load=sum(load),
                                solar=sum(solar,na.rm=T))) %>%
  mutate(cp=coal/load,gp=gas/load)
rwta<-as.data.frame(dta %>% group_by(region) %>%
                      summarize(coal=sum(coal),gas=sum(gas),load=sum(load),
                                wind=sum(wind,na.rm=T))) %>%
  mutate(cp=coal/load,gp=gas/load)
rta<-data.frame(x=c(1,1,2,2),f=c('zCoal','aGas','zCoal','aGas'),
                y=c(weighted.mean(rsta$cp,rsta$solar),weighted.mean(rsta$gp,rsta$solar),
                    weighted.mean(rwta$cp,rwta$wind),weighted.mean(rwta$gp,rwta$wind))*100) %>%
  inner_join(aggregate(y~x,data=.,sum) %>% rename(tot=y)) %>%
  mutate(lab=paste(round(y),"%",sep=""),labY=ifelse(f=="zCoal",y+2,tot+2))

fgr<-function(z,ttl) {
  a<-base(z)+ggtitle(paste("Fossil Fuel Shares Weighted by\n",ttl," Renewable Output",sep=""))+
    coord_cartesian(ylim=c(0,60))+
    geom_col(aes(x,y,fill=f),width=0.7)+
    geom_text(aes(x,labY,color=f,label=lab),size=5)+noY+
    scale_fill_manual(values=c("slateblue","black"),guide="none")+
    scale_color_manual(values=c("slateblue","black"),guide="none")+
    scale_x_continuous(breaks=c(1,2),labels=c("Solar","Wind"))+
    theme(axis.title.x=element_blank())
  return(a)
}
ggsave("Figures/si_wind_solar_drivers_compare.jpg",
       plot=plot_grid(fgr(hta,"Hourly")+
                        ant("Coal",1,10.5,5,'white')+
                        ant("Gas",1,40,5,'white'),
                      fgr(rta,"Regional")),width=8,height=4)

# hourly by regions
# by hour
hrsta<-as.data.frame(dta %>% group_by(region,local_hour) %>%
                       summarize(coal=sum(coal),gas=sum(gas),load=sum(load),
                                 solar=sum(solar,na.rm=T)) %>%
                       mutate(cp=coal/load,gp=gas/load) %>% group_by(region) %>%
                       summarize(solar_coal=weighted.mean(cp,solar),solar_gas=weighted.mean(gp,solar)))
hrwta<-as.data.frame(dta %>% group_by(region,local_hour) %>%
                       summarize(coal=sum(coal),gas=sum(gas),load=sum(load),
                                 wind=sum(wind,na.rm=T)) %>%
                       mutate(cp=coal/load,gp=gas/load) %>% group_by(region) %>%
                       summarize(wind_coal=weighted.mean(cp,wind),wind_gas=weighted.mean(gp,wind)))
hta<-inner_join(hrsta,hrwta) %>% filter(!is.nan(wind_coal),!is.nan(solar_coal)) # just the 3 regions
fta<-data.frame(x=c(rep(c(1,4,7),times=2),rep(c(2,5,8),times=2)),
                fl=rep(rep(c('b','a'),each=3),times=2),
                y=c(hta$solar_coal,hta$solar_gas,hta$wind_coal,hta$wind_gas)*100) %>%
  mutate(lab=ifelse(y>0.5,paste(round(y),"%",sep=""),"<1%")) %>%
  inner_join(aggregate(y~x,data=.,sum) %>% rename(tot=y)) %>%
  mutate(ly=ifelse(fl=="b",y+2,tot+2))
ggsave("Figures/si_wind_solar_drivers_regional_compare.jpg",
       plot=base(fta)+geom_col(aes(x,y,fill=fl))+
         geom_text(aes(x,ly,label=lab,color=fl))+
         scale_fill_manual(values=c("slateblue","black"),guide="none")+
         scale_color_manual(values=c("slateblue","black"),guide="none")+
         scale_x_continuous(breaks=c(1,2,4,5,7,8),labels=rep(c("Solar","Wind"),times=3))+
         xlab("       NE                                     TEX                                  WECC   ")+
         ant("Gas",4,40,5,'white')+ant("Coal",4,8,5,'white')+
         ylab("Weighted\nFossil Fuel\nShare\n(%)"),width=8,height=4)
