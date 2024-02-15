# script to generate Figure 5

# set WD accordingly
setwd("~/Desktop/AQ/Data_Scripts/")
source("Scripts/prep.R")

# figure comparing damages across approaches
dta <- read.csv("Data/f5_data.csv")

fta<-data.frame(x=c(0.8,1.8,3.8,4.8,6.8,7.8,
                    1,2,4,5,7,8,
                    1.2,2.2,4.2,5.2,7.2,8.2),
                fll=rep(c("aCurrent","bFJ","cAVERT"),each=6),
                y=c(dta$this_work,dta$fell_johnson,dta$epa_avert))
lb1<-data.frame(x=c(1.475,4.48,7.5),y=0.79,lb=c("CO","SO","NOx"))
lb2<-data.frame(x=c(1.5,4.5,7.5),y=0.73,lb=c("(tons/MWh)","(kg/MWh)","(kg/MWh)"))
ggsave("Figures/f5_damages.jpg",
       plot=base(fta)+coord_cartesian(xlim=c(0.4,8.6),ylim=c(0,0.8))+
         geom_vline(xintercept=c(3,6),color='gray50',linetype="dotted")+
         geom_col(aes(x,y,fill=fll))+
         ant("2",1.81,0.78,4,'gray50')+ant("2",4.81,0.78,4,'gray50')+
         scale_fill_manual(values=c("navy","darkorange","slateblue"),labels=c("This work","Fell & Johnson (2021)","EPA AVERT"),name=NULL)+
         scale_x_continuous(breaks=c(1,2,4,5,7,8),labels=rep(c("Wind","Solar"),times=3))+
         geom_text(data=lb1,aes(x,y,label=lb),color='gray50',size=6)+
         geom_text(data=lb2,aes(x,y,label=lb),color='gray50',size=4)+
         ylab("Avoided Emissions\nper MWh\nWind or Solar")+
         scale_y_continuous(breaks=seq(0,0.8,0.2),labels=c("0",seq(0.2,0.8,0.2)))+
         theme(axis.title.x=element_blank(),legend.position="bottom"),width=8,height=4)
