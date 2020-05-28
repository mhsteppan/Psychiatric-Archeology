library("mclust")
library("viridis")  
library("RColorBrewer")
library("scales")
library("ggplot2")


d<-read.csv(file="UScorrelations.csv")
#d<-read.csv(file="UKcorrelations.csv")

pd<-scale(d[,8:17])
mc<-Mclust(d[,8:17])

grenzen<-diff(mc$classification)/diff(mc$classification)
grenzen[is.na(grenzen)]<-0
x<-(diff(sma(grenzen,h=5)$fitted))



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


gg<-localmax(x,20)



s<-rowMeans(pd)

lm<-localmax(s,20)

p<-ggplot(data=balken)

r<-data.frame(matrix(NA,nrow=309,ncol=2))
for (i in 1:310){
  r[i,1]<-i+1699
  r[i,2]<-colnames(pd)[(which.max(pd[i,]))]
  r[i,3]<-pd[i,which.max(pd[i,])]
  r[i,4]<-s[i]
}



display.brewer.all()


show_col(hue_pal()(20))

p<-ggplot()
p<-p+geom_tile(aes(x=r[,1],y="American English",fill=r[,2]))
p<-p+xlim(1800,2015)

col<-hue_pal()(10)[c(1,7,8,6,9,1,5,4,3,2)]
col[8]<-"orange"
col[1]<-"brown"
#col[6]<-"purple"
col[3]<-"purple"
col[4]<-"cadetblue3"
p<-p+scale_fill_manual(values=col)

p<-p+geom_line(aes(x=r[,1],y=r[,4]/7+1),size=1,color="yellow")
#.p<-p+geom_point(aes(x=r[,1],y=mc$classification/30+1.5))

p<-p+geom_segment(data = data.frame(gg), aes(x =gg+1700, y = 0.5, xend = gg+1700, yend = 1.5))

for (i in 1:length(gg)){
  p<-p+annotate("text",x=gg[i]+1700,y=1.57,label=gg[i]+1700)
}

for (i in 1:(length(lm)-1)){
  p<-p+annotate("text",x=lm[i]+1700,y=s[lm[i]]/7+1.05,label=lm[i]+1700,color="yellow")
}
p<-p+xlab("Year")


