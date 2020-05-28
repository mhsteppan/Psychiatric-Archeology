setwd("C:/Users/Acer/Downloads/NgramScript-master/NgramScript-master")

uk<-read.csv("Supplementary Table 2 - Data British English.csv")
us<-read.csv("Supplementary Table 1 - Data American English.csv")
adj<-read.csv("adjectives.csv",sep=";")
time<-seq(1700,2025,25)

r<-data.frame(matrix("",nrow=13,ncol=3),stringsAsFactors = F)

n<-0
for (i in 2:length(time)){
  n<-n+1
  
  dd<-uk[which(uk[,2]>=time[i-1] & uk[,2]<time[i]),]
  aa<-aggregate(dd[,4],by=list(dd[,5]),FUN=mean)
  
  aa<-aa[order(aa[,2]*-1)[1:15],]
  xxx<-matrix()
  
  for (j in 1:nrow(aa)){
    x<-adj[which(adj[,1]==as.character(aa[j,1])),]
    xx<-which.max(abs(x[,2:6]))
    if (x[,xx+1]>0){ xxx<-c(xxx,paste(names(xx),"+",sep=""))}
    if (x[,xx+1]<0){ xxx<-c(xxx,paste(names(xx),"-",sep=""))}
  }
  xxx<-xxx[-1]
  
  r[n,1]<-as.character(paste(paste(time[i-1],"-",time[i]-1)),collapse="")
  r[n,2]<-as.character(paste(paste(aa[,1]," (",as.character(xxx),")",sep=""),sep="",collapse=", "),sep=" ")
  
  dd<-us[which(us[,2]>time[i-1] & us[,2]<time[i]),]
  aa<-aggregate(dd[,4],by=list(dd[,5]),FUN=mean)
  
  aa<-aa[order(aa[,2]*-1)[1:15],]
  xxx<-matrix()
  
  for (j in 1:nrow(aa)){
    x<-adj[which(adj[,1]==as.character(aa[j,1])),]
    xx<-which.max(abs(x[,2:6]))
    if (x[,xx+1]>0){ xxx<-c(xxx,paste(names(xx),"+",sep=""))}
    if (x[,xx+1]<0){ xxx<-c(xxx,paste(names(xx),"-",sep=""))}
  }
  xxx<-xxx[-1]
  
  r[n,3]<-as.character(paste(paste(aa[,1]," (",as.character(xxx),")",sep=""),sep="",collapse=", "),sep=" ")
  
  
}

r<-r[-c(1:4),]
names(r)<-c("Period","British English","American English")

r<-r[,c(1,3,2)]


