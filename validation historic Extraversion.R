setwd("C:/Users/Acer/Downloads/NgramScript-master/NgramScript-master")
d<-read.csv(file="UScorrelations.csv")

e1<-read.csv(file="C:/Users/Acer/Downloads/birth cohort extraversion 1mm.txt",sep=";",header=F)
e2<-read.csv(file="C:/Users/Acer/Downloads/birth cohort extraversion 2f.csv",sep=";",header=F)

e<-rbind(e1,e2)

bench<-scale(d$E)
mmpi<-read.csv(file="C:/Users/Acer/Downloads/mmpi historic.csv",header=T)

span<-c(1938,2007)
hypom<-mean(bench[239:308])
hypos<-sd(bench[239:308])
em<-mean(bench[265:293])

es<-sd(bench[265:293])


d$flynn<-NA
d$flynn[round(e[,1],digits=0)-1700]<-e[,2]

d$flynn2<-NA
d$flynn2[mmpi[,1]-1700]<-mmpi$Hypomania


mi<-min(e[,1])
ma<-max(e[,1])

c<-cor.test(bench[(mi-1700):(ma-1700)],seq(mi,ma,1))

c2<-cor.test(e[,1],e[,2])



e3<-scale(mmpi$Hypomania)

p<-ggplot()

p<-p+geom_rect(aes(xmin = mi, xmax = ma, ymin=-Inf, ymax=Inf), alpha=0.6, fill="grey90")
p<-p+ geom_vline(aes(xintercept=mi), linetype="dashed") 
p<-p+ geom_vline(aes(xintercept=ma), linetype="dashed")

p<-p+geom_point(aes(x=e[,1],y=scale(e[,2])*es+em,color="EPI Extraversion \n (Twenge, 2001)"),span=0.3,size=0.5)
p<-p+geom_smooth(aes(x=e[,1],y=scale(e[,2])*es+em,color="EPI Extraversion \n (Twenge, 2001)"),span=0.3,se=F,method="lm")
#p<-p+geom_point(aes(x=mmpi[,1],y=scale(mmpi$Hypomania)*hypos+hypom,color="MMPI Hypomania"),span=0.2,size=0.5)
#p<-p+geom_smooth(aes(x=mmpi[,1],y=scale(mmpi$Hypomania)*hypos+hypom,color="MMPI Hypomania"),span=0.5,se=F,method="lm")
p<-p+geom_line(aes(x=d$year,y=scale(d$E),color=" This study Extraversion"),size=1)
p<-p+xlim(1800,2010)
p<-p+theme_Publication()
p<-p+scale_color_manual(name="", values=c("black","indianred1","blue"))
p<-p+xlab("Year")+ylab("z score")
p<-p+ggtitle("Extraversion")            

#p<-p+annotate("text",x=1975,y=2,label=paste("r=",round(c$estimate,digits=2),"***","\n", "p<",formatC(c$p.value,format="e",digits=2)),color="red")
#p<-p+annotate("text",x=1975,y=1,label=paste("r=",round(c2$estimate,digits=2),"***","\n", "p<",formatC(c2$p.value,format="e",digits=2)),color="blue")

p<-p+annotate("text",x=(mi+ma)/2,y=-2.2,label=paste("r=0.50","** ","\n"),color="black")
p<-p+annotate("text",x=(mi+ma)/2,y=-2.5,label=paste("r=",round(c2$estimate,digits=2),"***","\n"),color="indianred1")




theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

