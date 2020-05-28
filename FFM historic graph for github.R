library("smooth")
library("ggrepel")
library("ggsci")
library("cowplot")
library("tidyquant")


watson<-read.csv("presidentialWatson.csv",header=T)


uk<-read.csv("Supplementary Table 2 - Data British English.csv")
us<-read.csv("Supplementary Table 1 - Data American English.csv")
adj<-read.csv("adjectives.csv",sep=";")
time<-seq(1700,2025,25)

r<-data.frame(matrix(NA,nrow=0,ncol=5))
#uk[which(uk[,2]<1800),3]<-NA
#us[which(us[,2]<1800),3]<-NA

for (i in 1:310){
  

  if (i > 100){
  cuk<-mean(uk[which(uk[,2]==i+1699),3])
  
  r[i,1]<-as.numeric(cuk)
  
  tt<-t.test(uk[which(uk[,2]==i+1699),3],uk[which(uk[,2]!=i+1699),3])
  r[i,2]<-tt$p.value
  
  cus<-mean(us[which(us[,2]==i+1699),3])
  
  tt<-t.test(us[which(us[,2]==i+1699),3],us[which(us[,2]!=i+1699),3])
  r[i,3]<-cus
  r[i,4]<-tt$p.value
}
  }



titre<-c("Agreeableness","Extraversion","Conscientiousness","Neuroticism","Openness")

a<-ggplot()
b<-ggplot()
plots=list()



for (kk in 1:1){


  
r<-data.frame(matrix(NA,nrow=0,ncol=4))



  
for (i in 1:310){
 
  cuk<-cor.test(uk[which(uk[,2]==i+1699),4],uk[which(uk[,2]==i+1699),kk+5])
  
  
  r[i,1]<-as.numeric(cuk$estimate)
  r[i,2]<-as.numeric(cuk$p.value)
  
  cus<-cor.test(us[which(us[,2]==i+1699),4],us[which(us[,2]==i+1699),kk+5])
  
  r[i,3]<-cus$estimate
  r[i,4]<-cus$p.value
  
  
}

r$watsonA<-NA
r[(watson$year-1700),"watsonA"]<-watson$big5_agreeableness
r$watsonC<-NA
r[(watson$year-1700),"watsonC"]<-watson$big5_conscientiousness
r$watsonN<-NA
r[(watson$year-1700),"watsonN"]<-watson$big5_neuroticism
r$watsonO<-NA
r[(watson$year-1700),"watsonO"]<-watson$OpennessRepaired
r$watsonE<-NA
r[(watson$year-1700),"watsonE"]<-watson$big5_extraversion


smuk<-sma(r[,1],h=25)$fitted[,1]
smus<-sma(r[,3],h=25)$fitted[,1]



if (kk==1) {smw<-sma(r$watsonA,h=25)$fitted[,1]}
if (kk==2) {smw<-sma(r$watsonE,h=25)$fitted[,1]}
if (kk==3) {smw<-sma(r$watsonC,h=25)$fitted[,1]}
if (kk==4) {smw<-sma(r$watsonN,h=25)$fitted[,1]}
if (kk==5) {smw<-sma(r$watsonO,h=25)$fitted[,1]}



print(smuk[1])

colnames(r)[c(1,3)]<-c("V1","V3")
siguk<-which(r[,2]<0.05)
sigus<-which(r[,4]<0.05)

lminuk<-localmin(smuk,10)
lmaxuk<-localmax(smuk,10)
lminus<-localmin(smus,10)
lmaxus<-localmax(smus,10)


dd<-data.frame(smuk)
dd[,2]<-NA
dd[lminuk,2]<-lminuk+1699
dd[,3]<-NA
dd[lmaxuk,3]<-lmaxuk+1699
dd[,4]<-NA
dd[,4]<-smus
dd[,5]<-NA
dd[lminus,5]<-lminus+1699
dd[,6]<-NA
dd[lmaxus,6]<-lmaxus+1699

#c1<-cor.test(r[100:310,1],r[100:310,3])
c1<-cor.test(dd[100:310,1],dd[100:310,4])
c1suff<-""
if (c1$p.value<0.05){c1suff<-"*"}
if (c1$p.value<0.01){c1suff<-"**"}
if (c1$p.value<0.001){c1suff<-"***"}


if (kk==1){out<-watson$big5_agreeableness}
if (kk==3){out<-watson$big5_conscientiousness}
if (kk==2){out<-watson$big5_extraversion}
if (kk==4){out<-watson$big5_neuroticism}
if (kk==5){out<-watson$OpennessRepaired}

c2<-cor.test(out,dd[watson$year-1700,4])
c2suff<-""
if (c2$p.value<0.05){c2suff<-"*"}
if (c2$p.value<0.01){c2suff<-"**"}
if (c2$p.value<0.001){c2suff<-"***"}

r$out[watson$year-1700]<-out


wa<-sma(r$out,h=25)$fitted[,1]



p<-ggplot()




for (k in 1:length(siguk)){
  
  p<-p+annotate("point",x=siguk[k]+1699,y=smuk[siguk[k]],color="orange",size=3.5)

 
}



for (k in 1:length(sigus)){
 
  p<-p+annotate("point",x=sigus[k]+1699,y=smus[sigus[k]],color="orange",size=3.5)

 
}







p<-p+geom_line(aes(x=seq(1700,2009,1),y=as.numeric(smuk[1:310]),color="British English"),size=1.5)
#p<-p+geom_smooth(aes(x=seq(1700,2009,1),y=as.numeric(r[,1]),color="British English"),span=0.1,se=F)

p<-p+geom_line(aes(x=seq(1700,2009,1),y=as.numeric(smus[1:310]),color="American English"),size=1.5)
p<-p+ggtitle(titre[kk])

p<-p+annotate("text",x=1900,y=0.14,label=paste("r(UK,US) = ", round(c1$estimate,digits=2),c1suff))

if (c2$estimate<0){
p<-p+annotate("text",x=1900,y=0.12,label=paste("r(US,Watson) = ", round(c2$estimate,digits=2),c2suff))
}

if (c2$estimate>0){
  p<-p+annotate("text",x=1900,y=0.12,label=paste("r(US,Watson) = ", round(c2$estimate,digits=2),c2suff))
}


if (kk==1){
  
  p<-p+geom_point(aes(x=watson$year,y=scale(watson$big5_agreeableness)*sd(dd[,1]),color="Inaugural speeches  (IBM Watson)"))
  #p<-p+geom_smooth(aes(x=watson$year,y=scale(watson$big5_agreeableness)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"),se=F,linetype="dashed")
 
  
   
}

if (kk==2){
  
  p<-p+geom_point(aes(x=watson$year,y=scale(watson$big5_extraversion)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"))
  #p<-p+geom_smooth(aes(x=watson$year,y=scale(watson$big5_extraversion)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"),se=F,linetype="dashed")
  
}

if (kk==3){
  
  p<-p+geom_point(aes(x=watson$year,y=scale(watson$big5_conscientiousness)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"))
  #p<-p+geom_smooth(aes(x=watson$year,y=scale(watson$big5_conscientiousness)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"),se=F,linetype="dashed")
  
}

if (kk==4){
  
  p<-p+geom_point(aes(x=watson$year,y=scale(watson$big5_neuroticism)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"))
  #p<-p+geom_smooth(aes(x=watson$year,y=scale(watson$big5_neuroticism)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"),se=F,linetype="dashed")
  
}

if (kk==5){
  
  p<-p+geom_point(aes(x=watson$year,y=scale(watson$OpennessRepaired)*sd(dd[,1]),color="Inaugural speeches  (IBM Watson)"))
  #p<-p+geom_smooth(aes(x=watson$year,y=scale(watson$OpennessRepaired)*sd(dd[,1]),color="Presidential speeches  (IBM Watson)"),se=F,linetype="dashed")
  
}


p<-p+geom_smooth(aes(x=seq(1700,2016,1),y=scale(r$out)*sd(dd[,1]),color="Inaugural speeches  (IBM Watson)"),se=F,linetype="dashed")



#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,1]-0.05,label=dd[,2],color="British English"))
#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,1]+0.05,label=dd[,3],color="British English"))
#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,4]-0.05,label=dd[,5],color="American English"))
#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,4]+0.05,label=dd[,6],color="American English"))



p<-p+theme_bw()
p<-p+scale_color_manual(values=c("gray20","gray80","blue"))
p<-p+ylab("Correlation")
p<-p+xlab("Year")+theme_Publication() 
p<-p+labs(color="")

p<-p+xlim(c(1800,2010))
p<-p+ylim(c(-0.15,0.15))

p <- p + scale_y_continuous(sec.axis = sec_axis(~ . * sd(dd[,1]),name = "z score"))
p<-p+scale_fill_Publication()


pp<-p

#p<-p+geom_text_repel(aes(x=lminuk[k]+1699,y=smuk[lminuk[k]]-0.05,label=lminuk[k]+1699))

if (kk==1){a<-pp+ theme(legend.position="none")}
if (kk==2){b<-pp+ theme(legend.position="none")}
if (kk==3){c<-pp+ theme(legend.position="none")}
if (kk==4){d<-pp+ theme(legend.position="none")}
if (kk==5){e<-pp+ theme(legend.position="none")}



plots[[kk]]<-pp
print(plots[[kk]])



}


legend <- get_legend(
  # create some space to the left of the legend
  a + theme(legend.box.margin = margin(0, 0, 0, 12))
)


plot_grid(a, b,c,d,e, labels = c('A', 'B','C','D','E'),nrow=3)


  r<-data.frame(matrix(NA,nrow=0,ncol=4))

for (i in 1:310){
  
  cuk<-cor.test(uk[which(uk[,2]==i+1699),4],uk[which(uk[,2]==i+1699),kk+5])
  tt<-t.test(uk[which(uk[,2]==i+1699),3],uk[which(uk[,2]!=i+1699),3])
  
  r[i,1]<-mean(uk[which(uk[,2]==i+1699),3])
  r[i,2]<-tt$p.value
  
  cus<-cor.test(us[which(us[,2]==i+1699),4],us[which(us[,2]==i+1699),kk+5])
  tt<-t.test(us[which(us[,2]==i+1699),3],us[which(us[,2]!=i+1699),3])
  
  
  r[i,3]<-mean(us[which(us[,2]==i+1699),3])
  r[i,4]<-tt$p.value
  
  
}

smuk<-sma(r[,1],h=5)$fitted[,1]
smus<-sma(r[,3],h=5)$fitted[,1]


print(smuk[1])

colnames(r)[c(1,3)]<-c("V1","V3")
siguk<-which(r[,2]<0.05)
sigus<-which(r[,4]<0.05)

lminuk<-localmin(smuk,10)
lmaxuk<-localmax(smuk,10)
lminus<-localmin(smus,10)
lmaxus<-localmax(smus,10)


dd<-data.frame(smuk)
dd[,2]<-NA
dd[lminuk,2]<-lminuk+1699
dd[,3]<-NA
dd[lmaxuk,3]<-lmaxuk+1699
dd[,4]<-NA
dd[,4]<-smus
dd[,5]<-NA
dd[lminus,5]<-lminus+1699
dd[,6]<-NA
dd[lmaxus,6]<-lmaxus+1699




r$time<-seq(1700,2009,1)

p<-ggplot()
p<-p+geom_line(aes(x=seq(1700,2009,1),y=as.numeric(dd[,1]),color="British English"),size=1.5)
#p<-p+geom_smooth(aes(x=seq(1700,2009,1),y=as.numeric(r[,1]),color="British English"),span=0.1,se=F)

p<-p+geom_line(aes(x=seq(1700,2009,1),y=as.numeric(dd[,4]),color="American English"),size=1.5)
p<-p+ggtitle("All personality adjectives")


for (k in 1:length(siguk)){
  p<-p+annotate("point",x=siguk[k]+1699,y=smuk[siguk[k]],color="orange",size=3)
  
}

for (k in 1:length(sigus)){
  p<-p+annotate("point",x=sigus[k]+1699,y=smus[sigus[k]],color="orange",size=3)
  
}

#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,1]-0.05,label=dd[,2],color="British English"))
#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,1]+0.05,label=dd[,3],color="British English"))
#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,4]-0.05,label=dd[,5],color="American English"))
#p<-p+geom_text_repel(aes(x=seq(1700,2009,1),y=dd[,4]+0.05,label=dd[,6],color="American English"))



p<-p+theme_bw()
p<-p+scale_color_manual(values=c("black","gray"))
p<-p+ylab("Average ngram score")
p<-p+xlab("Year")+theme_Publication() 
p<-p+labs(color="")

p<-p+xlim(c(1800,2010))

pp<-p




plot_grid(a, b, c, d, e,
          labels = c("A", "B", "C","D","E"),
          ncol = 2, nrow = 3)




localmax<-function(x,window,npar=TRUE,print=FALSE) {
  result<-matrix()
  
  for (i in (1+window):(length(x)-window))
  {
    
    temp<-seq(i-window,i+window,1)
    if (which.max(x[temp])==window+1)
    {
      result<-c(result,i)
    }
  }
  
  
  result<-c(result,i+which.max(x[(length(x)-window):length(x)])-1)
  return(result[-1])
  
}



localmin<-function(x,window,npar=TRUE,print=FALSE) {
  result<-matrix()
  
  for (i in (1+window):(length(x)-window))
  {
    
    temp<-seq(i-window,i+window,1)
    if (which.min(x[temp])==window+1)
    {
      result<-c(result,i)
    }
  }
  
  return(result[-1])
  
}



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
            panel.grid.major = element_blank(),
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

