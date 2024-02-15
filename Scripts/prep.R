# prep script, uploaded in other scripts

library(tidyverse)
library(lubridate)
library(cowplot)


# functions
base<-function(z) {
  ggplot(data=z)+theme_light()+theme(axis.text=element_text(size=14,color='gray50'),axis.title=element_text(size=14,color='gray50',angle=0),axis.title.y=element_text(size=14,color='gray50',angle=0),axis.ticks=element_blank(),
                                     panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.title=element_text(size=14,face='bold',color='gray50',hjust=0.5),
                                     panel.border = element_blank(), axis.line=element_line(color='gray50'),legend.title=element_text(size=14,face='bold',color='gray50'),legend.text=element_text(size=14,color='gray50'))
}
lblblu<-rgb(8/255,48/255,106/255)
nrelblu<-rgb(0/255,112/255,180/255)
nvy<-rgb(26/255,71/255,111/255)
noX<-theme(axis.title.x=element_blank(),axis.text.x=element_blank())
noY<-theme(axis.title.y=element_blank(),axis.text.y=element_blank())
p25<-function(z) quantile(z,.25,na.rm=T)
p75<-function(z) quantile(z,.75,na.rm=T)
ant<-function(lb,x,y,sz,cl) annotate('text',x=x,y=y,label=lb,size=sz,color=cl)
gs<-function(x1,x2,y1,y2,cl,sz=0.3) geom_segment(x=x1,xend=x2,y=y1,yend=y2,color=cl,size=sz)

# regression clean function
regClean<-function(R,lbz) { # R is the SUMMARY, should be standard-ish, lbz are variable names
  x1<-data.frame(coeff=R[,1],se=R[,2],p=R[,4]) %>%
    mutate(result=paste(round(coeff,2),ifelse(p<0.05,"*",""),sep=""),
           se_clean=paste("'(",round(se,2),")",sep=""))
  x2<-rbind(data.frame(x=seq(1,nrow(R)*2-1,2),var=lbz,result=x1$result),
            data.frame(x=seq(2,nrow(R)*2,2),var="",result=x1$se_clean)) %>% arrange(x) %>% select(-x)
  return(x2)
}

# figure base for national figures
fgBase<-function(d,ttl,yM,y1,y2,y3,yG,yC,yNox,ySo,yCo,grdd,nxc) {
  if (yM%%2==0) ymx<-yM
  else ymx<-yM-1
  a<-base(d)+ggtitle(ttl)+
    geom_col(aes(pos,y*10^-9,fill=fl,alpha=f),color='white',width=0.7)+coord_cartesian(xlim=c(0.6,3.4),ylim=c(0,yM))+
    geom_segment(aes(x=pos-0.32,xend=pos-0.32,y=tlo*10^-9,yend=thi*10^-9),color='firebrick4')+
    geom_segment(data=filter(qqa,x==ttl),aes(x=0.68,xend=0.73,y=yy,yend=yy),color='firebrick4')+
    geom_text(aes(x=pos,y=labPos*10^-9,label=lb),color="white",size=3.5)+
    geom_text(data=filter(qqa,x==ttl),aes(x=0.64,y=yy,label=lb1),color='firebrick4',size=2.4,hjust=1)+
    geom_text(data=filter(qqa,x==ttl),aes(x=0.74,y=yyy,label=lb2),color='firebrick4',size=2.4,hjust=0)+
    scale_y_continuous(breaks=c(0,ymx/2,ymx))+
    scale_x_continuous(breaks=c(1:3),labels=c("Total","Pollutants","Regions"))+
    theme(axis.title.x=element_blank())+ylab("Avoided\nDamages\n($B)")+
    scale_alpha_manual(values=c(1,1),guide="none")+
    scale_fill_manual(values=grdd,guide="none")+scale_color_manual(values=grdd,guide="none")+
    theme(axis.title.x=element_blank())+
    geom_segment(x=1.65,xend=2.35,y=y2,yend=y2,color='gray50',linetype='dotted',linewidth=0.5)+
    geom_segment(x=1.61,xend=1.61,y=0,yend=y1,color='gray50',linewidth=0.3)+
    geom_segment(x=1.61,xend=1.64,y=0,yend=0,color='gray50',linewidth=0.3)+
    geom_segment(x=1.61,xend=1.64,y=y1,yend=y1,color='gray50',linewidth=0.3)+
    geom_segment(x=1.61,xend=1.61,y=y2,yend=y3,color='gray50',linewidth=0.3)+
    geom_segment(x=1.61,xend=1.64,y=y2,yend=y2,color='gray50',linewidth=0.3)+
    geom_segment(x=1.61,xend=1.64,y=y3,yend=y3,color='gray50',linewidth=0.3)+
    ant("Gas",1.5,yG,3,'gray50')+ant("Coal",1.49,yC,3,'gray50')+
    ant("NOx",2,yNox,4,nxc)+
    ant(bquote(SO[2]),2,ySo,4,'white')+ant(bquote(CO[2]),2,yCo,4,'white')
  return(a)
}

# YoY figures
bzz<-function(ps,dd,yM,grd) {
  if(yM<3) {
    yB<-yM
    ybks<-c(0,yB/2,yB)
    ylbs<-c("0",yB/2,yB)
  } 
  else {
    if(yM%%2==0) yB<-yM
    else yB<-yM-1
    ybks<-c(0,yB/2,yB)
    ylbs<-c(0,yB/2,yB)
  }
  a<-base(filter(dd,pos==ps))+coord_cartesian(xlim=c(2018.6,2022.4),ylim=c(0,yM))+
    geom_col(aes(year,y*10^-9,fill=fl,alpha=f),color='white',width=0.7)+
    scale_y_continuous(breaks=ybks,labels=ylbs)+scale_x_continuous(breaks=c(2019:2022))+
    theme(axis.title.x=element_blank())+ylab("Avoided\nDamages\n($B)")+
    scale_alpha_manual(values=c(1,1),guide="none")+scale_fill_manual(values=grd,guide="none")+scale_color_manual(values=grd,guide="none")
  return(a)
}
yoyFgr<-function(d,ttl,yM,y1,y2,y3,yG,yC,yNox,ySo,yCo,p2g,p3g,anott,nxc) {
  plot_grid(bzz(1,d,yM,'slategrey')+ggtitle(paste(ttl,": Total Avoided Damages",sep=""))+
              geom_text(data=filter(lbz,x==ttl),aes(year,ty,label=tlb),color='white',size=4.5)+
              geom_text(data=filter(lbz,x==ttl),aes(year,my,label=mlb),color='white',size=3.5),
            bzz(2,d,yM,p2g)+ggtitle(paste(ttl,": Pollutant Breakdown",sep=""))+
              geom_text(aes(x=year,y=labPos*10^-9,label=lb),color="white",size=3.5)+theme(axis.title.y=element_text(color='white'))+
              geom_segment(x=2021.65,xend=2022.35,y=y2,yend=y2,color='gray50',linetype='dotted',linewidth=0.5)+
              geom_segment(x=2021.61,xend=2021.61,y=0,yend=y1,color='gray50',linewidth=0.3)+
              geom_segment(x=2021.61,xend=2021.64,y=0,yend=0,color='gray50',linewidth=0.3)+
              geom_segment(x=2021.61,xend=2021.64,y=y1,yend=y1,color='gray50',linewidth=0.3)+
              geom_segment(x=2021.61,xend=2021.61,y=y2,yend=y3,color='gray50',linewidth=0.3)+
              geom_segment(x=2021.61,xend=2021.64,y=y2,yend=y2,color='gray50',linewidth=0.3)+
              geom_segment(x=2021.61,xend=2021.64,y=y3,yend=y3,color='gray50',linewidth=0.3)+
              ant("Gas",2021.5,yG,3,'gray50')+ant("Coal",2021.49,yC,3,'gray50')+
              ant("NOx",2022,yNox,4,nxc)+
              ant(bquote(SO[2]),2022,ySo,4,'white')+ant(bquote(CO[2]),2022,yCo,4,'white'),
            bzz(3,d,yM,p3g)+ggtitle(paste(ttl,": Regional Breakdown",sep=""))+anott+
              geom_text(aes(x=year,y=labPos*10^-9,label=lb),color="white",size=3.5)+theme(axis.title.y=element_text(color='white')),
            nrow=3)
}

# var uncertainty funnel figure
vFunnel<-function(dda,t2,ym,yM,ct,yShft) {
  base(dda[[1]])+coord_flip(ylim=c(ym,yM),xlim=c(-0.5,11.5))+
    geom_vline(xintercept=sg1,color='gray50')+
    geom_vline(xintercept=sg2,color='gray80')+
    geom_segment(aes(x=pos,xend=pos,y=lo,yend=hi,color=fl),linewidth=6)+
    scale_color_manual(values=c("firebrick4","slategrey","midnightblue","bisque4"),guide="none")+
    geom_text(data=filter(dda[[1]],!(x%in%c(10,9,6,3))),aes(pos,ly+yShft,label=lb),color='gray50',hjust=0)+
    geom_text(data=filter(dda[[1]],x%in%c(10,9,6,3)),aes(pos,ly,label=lb,color=fl),fontface='bold',hjust=0)+
    geom_text(data=filter(dda[[2]],t==1),aes(x,y,label=lb),fontface="bold",color='gray50',hjust=0)+
    geom_text(data=filter(dda[[2]],t==2),aes(x,y,label=lb),fontface="bold",color='gray50')+
    geom_text(data=filter(dda[[1]],df>ct),aes(pos,p5x,label=p5l),color='white',size=3,hjust=0)+
    geom_text(data=filter(dda[[1]],df<=ct),aes(pos,p5x,label=p5l,color=fl),size=3,hjust=1)+
    geom_text(data=filter(dda[[1]],df>ct),aes(pos,p95x,label=p95l),color='white',size=3,hjust=1)+
    geom_text(data=filter(dda[[1]],df<=ct),aes(pos,p95x,label=p95l,color=fl),size=3,hjust=0)+
    geom_text(data=t2,aes(x,y,label=lb),color='gray50',size=2.5)+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
          axis.title.x=element_blank(),axis.text.x=element_blank(),
          axis.line.y=element_blank(),axis.line.x=element_blank())
}

