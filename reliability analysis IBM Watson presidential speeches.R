library("psych")
dd<-read.csv("presidentialWatson.csv",header=T)

psych::alpha(dd[,2:8])
psych::alpha(dd[,9:15])
psych::alpha(dd[,16:22])
psych::alpha(dd[,24:29])

# Reliability analysis on Openness shows negative correlations:
psych::alpha(dd[,30:36])

# Alpha-maximized scale that is used instead:
OpennessRepaired<-rowMeans(dd[,c(30,31,32)])

