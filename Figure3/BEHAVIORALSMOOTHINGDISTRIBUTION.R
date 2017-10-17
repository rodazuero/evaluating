#------------#
#Housekeeping#
#------------#

rm(list=ls(all=TRUE))

library(scatterplot3d)
library(questionr)
#Setting the size of the observations 
SIZE=950
#Loading the number of bootstrap repetitions
RR=1000

#Run the necessary functions and libraries
library(ggplot2)
library(gridExtra)
BEHAVIORAL70
#Multiplot
source('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/RfunctionsUsed/multiplot.R', echo=TRUE)

#----------------------------------------------------
#Define the parameters of the graphs to be generated#
#----------------------------------------------------
setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/")
setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/Bootstrapfilter/OLDESTIMATES/CurrentEstimates1000")

#0. Directory to store and save graphs
initialdir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/Graphs"

#0.1 Directory where the final graphs are going to be saved (combined ones)
finaldir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/Graphs/CombinedResults"

#1. Thickness of lines
sizegraphs=2 


#------------------------------------#
#Loading the optimal parameters found#
#------------------------------------#

#Loading skills
S0<-as.matrix(read.table("S0MATRIX.csv", sep=",", header=FALSE))
S1<-as.matrix(read.table("S1MATRIX.csv", sep=",", header=FALSE))
S2<-as.matrix(read.table("S2MATRIX.csv", sep=",", header=FALSE))

#Loading weights
W0<-as.matrix(read.table("W0MATRIX.csv", sep=",", header=FALSE))
W1<-as.matrix(read.table("W1MATRIX.csv", sep=",", header=FALSE))
W2<-as.matrix(read.table("W2MATRIX.csv", sep=",", header=FALSE))



#Storing images
setwd('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/Graphs')

#First step is to compute the weighted mean of each element
sum0=0
mean0=0
sum1=0
mean1=0
sum2=0
mean2=0

for (ii in 1:SIZE){
  mean0=0
  mean1=0
  mean2=0
  sum0=0
  sum1=0
  sum2=0
  for(rr in 1:RR){
    #t=0
    mean0=sum(mean0,S0[ii,rr]*W0[ii,rr],na.rm=TRUE)
    sum0=sum0+W0[ii,rr]*(1-is.na(S0[ii,rr]))
    #t=1
    mean1=sum(mean1,S1[ii,rr]*W1[ii,rr],na.rm=TRUE)
    sum1=sum1+W1[ii,rr]*(1-is.na(S1[ii,rr]))
    #t=2
    mean2=sum(mean2,S2[ii,rr]*W2[ii,rr],na.rm=TRUE)
    sum2=sum2+W2[ii,rr]*(1-is.na(S2[ii,rr]))
  }
  #Storing the mean in the RR+1 component of the vector
  S0[ii,RR+1]=mean0/sum0
  S1[ii,RR+1]=mean1/sum1
  S2[ii,RR+1]=mean2/sum2
  
  #Normalizing the weights
  W0[ii,1:RR]<-W0[ii,1:RR]/sum0
  W1[ii,1:RR]<-W1[ii,1:RR]/sum1
  W2[ii,1:RR]<-W2[ii,1:RR]/sum2
}


#Order observations according to the average skills. Lowest to highest
S0<-S0[order(S0[,RR+1]),]
S1<-S1[order(S1[,RR+1]),]
S2<-S2[order(S2[,RR+1]),]
W0<-W0[order(S0[,RR+1]),]
W1<-W1[order(S1[,RR+1]),]
W2<-W2[order(S2[,RR+1]),]



#Standarizing all the MEAN of individuals and the observations of skills in the filter in with respect to standard deviations above the mean
#First the mean of each observation
VECMEAN=(log(S2[1:SIZE,RR+1])-mean(log(S2[1:SIZE,RR+1])))/sqrt(var(log(S2[1:SIZE,RR+1])))

#And now each of the outputs of the smoothing distribution - particle filter stuff. 
S2STD=(log(S2)-mean(log(S2[1:SIZE,RR+1])))/sqrt(var(log(S2[1:SIZE,RR+1])))


sum2=0
SD=array(0,dim=c(SIZE,1))

#First need to compute the standardized mean

sum0=0
mean0=0
sum1=0
mean1=0
sum2=0
mean2=0


for (ii in 1:SIZE){
  sum2=0
  #Put in the RR+1 column the mean
  S2STD[ii,RR+1]=mean(S2STD[ii,1:RR],na.rm=TRUE)
  for(rr in 1:RR){
    sum2=sum2+sum(S2STD[ii,rr],-S2STD[ii,RR+1],na.rm=TRUE)^2
  }
  sum2=sqrt(sum2/(RR-1))
  SD[ii]=sum2
}



#Not smoothing
#With one line

a<-100*seq(1:SIZE)/SIZE
b<-VECMEAN
tempLOWER<-VECMEAN
tempUPPER<-VECMEAN

## Now doing it with literally the 5th and 95th percentile:

##First of all, getting the 5th and 95th percentile of the RR repetitions

P5<-round(RR*0.05)
P95<-round(RR*0.95)
for (ii in 1:SIZE){
  #Getting the order of the weights
  ORDENW<-order(S2[ii,1:RR])
  
  #Getting the corresponding percentiles 
  #5th percentile:
  sum5=0
  counter<-0
  while (sum5<0.05){
    counter<-counter+1
    sum5<-sum5+W2[ii,ORDENW[counter]]
  }
  tempLOWER[ii]<-S2STD[ii,ORDENW[counter]]
  #95th percentile:
  sum95=0
  counter<-0
  while (sum95<0.95){
    counter<-counter+1
    sum95<-sum95+W2[ii,ORDENW[counter]]
  }
  tempUPPER[ii]<-S2STD[ii,ORDENW[counter]]
}


DFGRAPH=data.frame(a,b,tempLOWER,tempUPPER)
p<-ggplot(DFGRAPH, aes(a,b))
p<-p+  geom_line(size=2, aes(colour=" Mean "))
p<-p+geom_ribbon(data=DFGRAPH,aes(ymin=tempLOWER,ymax=tempUPPER, fill="95% Confidence Interval"),alpha=0.3)
#p<-p+geom_line(size=2)
p<-p+labs(title="",x="Percentile", y="Skills-Standard Deviations")
p<-p+scale_colour_manual("", values = c("blue","red"))
p<-p+scale_fill_manual("",values="grey12")
pline<-p
dev.set()
pdf('Smoothingdistribution.pdf')
pline
dev.off()


#If we want to smooth the confidence intervals:
tempLOWER<-predict(loess(tempLOWER~a))
tempUPPER<-predict(loess(tempUPPER~a))

DFGRAPH=data.frame(a,b,tempLOWER,tempUPPER)
p<-ggplot(DFGRAPH, aes(a,b))
p<-p+  geom_line(size=2, aes(colour=" Mean "))
p<-p+geom_ribbon(data=DFGRAPH,aes(ymin=tempLOWER,ymax=tempUPPER, fill="95% Confidence Interval"),alpha=0.3)
#p<-p+geom_line(size=2)
p<-p+labs(title="",x="Percentile", y="Skills-Standard Deviations")
p<-p+scale_colour_manual("", values = c("blue","red"))
p<-p+scale_fill_manual("",values="grey12")
pline<-p
dev.set()
pdf('Smoothingdistribution2.pdf')
pline
dev.off()














#===================================#
#                                   #
#      SORTING IT WITH INCOME       #
#                                   #
#===================================#



rm(list=ls(all=TRUE))

library(scatterplot3d)
library(questionr)
#Setting the size of the observations 
SIZE=950
#Loading the number of bootstrap repetitions
RR=1000

#Run the necessary functions and libraries
library(ggplot2)
library(gridExtra)

#Multiplot
source('~/Documents/Research/Chile/RR/RfunctionsUsed/multiplot.R', echo=TRUE)

#----------------------------------------------------
#Define the parameters of the graphs to be generated#
#----------------------------------------------------
setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/")

#0. Directory to store and save graphs
initialdir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/Graphs"

#0.1 Directory where the final graphs are going to be saved (combined ones)
finaldir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/Graphs/CombinedResults"

#1. Thickness of lines
sizegraphs=2 


#------------------------------------#
#Loading the optimal parameters found#
#------------------------------------#

#Loading skills
S0<-as.matrix(read.table("S0MATRIX.csv", sep=",", header=FALSE))
S1<-as.matrix(read.table("S1MATRIX.csv", sep=",", header=FALSE))
S2<-as.matrix(read.table("S2MATRIX.csv", sep=",", header=FALSE))

#Loading weights
W0<-as.matrix(read.table("W0MATRIX.csv", sep=",", header=FALSE))
W1<-as.matrix(read.table("W1MATRIX.csv", sep=",", header=FALSE))
W2<-as.matrix(read.table("W2MATRIX.csv", sep=",", header=FALSE))


#Loading the income distribution to sort them according to that


#First doing it with the current situation
incsort<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/Counterfactuals/Original/IncomeDistributionWNAMES.csv", sep=",", header=TRUE)

#Now doing it with the whole stuff
USETOTINCEOMALT=0
if (USETOTINCEOMALT==1){
  e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/BEHAVIORALWNAMES.csv", quote="")
  data.Mwage12=data.matrix(e$Mwage12,rownames.force=NA)
  data.Fwage12=data.matrix(e$Fwage12,rownames.force=NA)
  data.Mwage10=data.matrix(e$Mwage10,rownames.force=NA)
  data.Fwage10=data.matrix(e$Fwage10,rownames.force=NA)
  data.Mnlincome12=data.matrix(e$Mnlincome12,rownames.force=NA)
  data.Fnlincome12=data.matrix(e$Fnlincome12,rownames.force=NA)
  data.Mnlincome10=data.matrix(e$Mnlincome10,rownames.force=NA)
  data.Fnlincome10=data.matrix(e$Fnlincome10,rownames.force=NA)
  data.Ffraclabor12=data.matrix(e$Ffraclabor12,rownames.force=NA)
  data.Mfraclabor12=data.matrix(e$Mfraclabor12,rownames.force=NA)
  data.Ffraclabor10=data.matrix(e$Ffraclabor10,rownames.force=NA)
  data.Mfraclabor10=data.matrix(e$Mfraclabor10,rownames.force=NA)
  TOTINCOME<-data.Mnlincome12+data.Fnlincome12+data.Mwage12*data.Mfraclabor12+data.Fwage12*data.Ffraclabor12
  
  #replacing value
  incsort$totincome10=TOTINCOME
}
#Storing images
setwd('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/advances/reports/Figures')

#First step is to compute the weighted mean of each element
sum0=0
mean0=0
sum1=0
mean1=0
sum2=0
mean2=0

for (ii in 1:SIZE){
  mean0=0
  mean1=0
  mean2=0
  sum0=0
  sum1=0
  sum2=0
  for(rr in 1:RR){
    #t=0
    mean0=sum(mean0,S0[ii,rr]*W0[ii,rr],na.rm=TRUE)
    sum0=sum0+W0[ii,rr]*(1-is.na(S0[ii,rr]))
    #t=1
    mean1=sum(mean1,S1[ii,rr]*W1[ii,rr],na.rm=TRUE)
    sum1=sum1+W1[ii,rr]*(1-is.na(S1[ii,rr]))
    #t=2
    mean2=sum(mean2,S2[ii,rr]*W2[ii,rr],na.rm=TRUE)
    sum2=sum2+W2[ii,rr]*(1-is.na(S2[ii,rr]))
  }
  #Storing the mean in the RR+1 component of the vector
  S0[ii,RR+1]=mean0/sum0
  S1[ii,RR+1]=mean1/sum1
  S2[ii,RR+1]=mean2/sum2
  
  #Normalizing the weights
  W0[ii,1:RR]<-W0[ii,1:RR]/sum0
  W1[ii,1:RR]<-W1[ii,1:RR]/sum1
  W2[ii,1:RR]<-W2[ii,1:RR]/sum2
}


#Order observations according to the average skills. Lowest to highest
S0<-S0[order(incsort$totincome12),]
S1<-S1[order(incsort$totincome12),]
S2<-S2[order(incsort$totincome12),]
W0<-W0[order(incsort$totincome12),]
W1<-W1[order(incsort$totincome12),]
W2<-W2[order(incsort$totincome12),]



#Standarizing all the MEAN of individuals and the observations of skills in the filter in with respect to standard deviations above the mean
#First the mean of each observation
VECMEAN=(log(S2[1:SIZE,RR+1])-mean(log(S2[1:SIZE,RR+1])))/sqrt(var(log(S2[1:SIZE,RR+1])))

#And now each of the outputs of the smoothing distribution - particle filter stuff. 
S2STD=(log(S2)-mean(log(S2[1:SIZE,RR+1])))/sqrt(var(log(S2[1:SIZE,RR+1])))


sum2=0
SD=array(0,dim=c(SIZE,1))

#First need to compute the standardized mean

sum0=0
mean0=0
sum1=0
mean1=0
sum2=0
mean2=0


for (ii in 1:SIZE){
  sum2=0
  #Put in the RR+1 column the mean
  S2STD[ii,RR+1]=mean(S2STD[ii,1:RR],na.rm=TRUE)
  for(rr in 1:RR){
    sum2=sum2+sum(S2STD[ii,rr],-S2STD[ii,RR+1],na.rm=TRUE)^2
  }
  sum2=sqrt(sum2/(RR-1))
  SD[ii]=sum2
}



#Not smoothing
#With one line

a<-100*seq(1:SIZE)/SIZE
b<-VECMEAN
tempLOWER<-VECMEAN
tempUPPER<-VECMEAN

## Now doing it with literally the 5th and 95th percentile:

##First of all, getting the 5th and 95th percentile of the RR repetitions

P5<-round(RR*0.05)
P95<-round(RR*0.95)
for (ii in 1:SIZE){
  #Getting the order of the weights
  ORDENW<-order(S2[ii,1:RR])
  
  #Getting the corresponding percentiles 
  #5th percentile:
  sum5=0
  counter<-0
  while (sum5<0.05){
    counter<-counter+1
    sum5<-sum5+W2[ii,ORDENW[counter]]
  }
  tempLOWER[ii]<-S2STD[ii,ORDENW[counter]]
  #95th percentile:
  sum95=0
  counter<-0
  while (sum95<0.95){
    counter<-counter+1
    sum95<-sum95+W2[ii,ORDENW[counter]]
  }
  tempUPPER[ii]<-S2STD[ii,ORDENW[counter]]
}



#Differences between poorest and richest chidren
mean(b[1:190])
mean(b[760:950])
#Ranking
RANKING<-rank(b)
mean(RANKING[1:190])/950
mean(RANKING[760:950])/950

mean(RANKING[1:95])/950
mean(RANKING[855:950])/950

eruption.lm = lm(b ~ a)
summary(eruption.lm)

DFGRAPH=data.frame(a,b,tempLOWER,tempUPPER)
p<-ggplot(DFGRAPH, aes(a,b))
p<-p+  geom_line(size=2, aes(colour=" Mean "))
p<-p+geom_ribbon(data=DFGRAPH,aes(ymin=tempLOWER,ymax=tempUPPER, fill="95% Confidence Interval"),alpha=0.3)
#p<-p+geom_line(size=2)
p<-p+labs(title="",x="Percentile", y="Skills-Standard Deviations")
p<-p+scale_colour_manual("", values = c("blue","red"))
p<-p+scale_fill_manual("",values="grey12")
pline<-p
dev.set()
pdf('SmoothingdistributionINCOME1.pdf')
pline
dev.off()


#If we want to smooth the confidence intervals:
spandeci<-0.9
degreedeci<-2
tempLOWERINC<-predict(loess(tempLOWER~a,degree=degreedeci,span=spandeci))
tempUPPERINC<-predict(loess(tempUPPER~a,degree=degreedeci,span=spandeci))
bINC<-predict(loess(b~a,degree=degreedeci,span=spandeci))
aINC<-a



DFGRAPH=data.frame(aINC,bINC,tempLOWERINC,tempUPPERINC)
p<-ggplot(DFGRAPH, aes(aINC,bINC))
p<-p+  geom_line(size=2, aes(colour=" Mean "))
p<-p+geom_ribbon(data=DFGRAPH,aes(ymin=tempLOWERINC,ymax=tempUPPERINC, fill="95% Confidence Interval"),alpha=0.3)
#p<-p+geom_line(size=2)
p<-p+labs(title="",x="Percentile (Income)", y="Skills-Standard Deviations")
p<-p+scale_colour_manual("", values = c("blue","red"))
p<-p+scale_fill_manual("",values="grey12")
p<-p+ylim(c(-0.6,0.6))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="right")
pline<-p
dev.set()
pdf('SmoothingdistributionINCOME2.pdf')
pline
dev.off()

#If doing it with png
dev.set()
png('SmoothingdistributionINCOME2.png',
    units="in",
    width=9,
    height=4,
    res=96)
pline
dev.off()



##============================================##
##  Otros intentos de smoothing distribution  ##
##============================================##

#library(scatterplot3d)
#scatterplot3d(dfT, type = "h", color = "blue",
#              angle = 50, scale.y = 0.7, xlim = c(1, 1000), ylim=c(-15,15),
#              zlim = c(0,0.02), pch = ".", main = " ",
#              xlab="Percentile X 10",ylab="log(Skills)",zlab="Density",
#              lab.z=1)







sortx<-incsort$totincome10
S0[order(incsort$totincome10),]
S2sort<-S2STD[order(sortx),]
W2sort<-W2[order(sortx),]
sortxsort<-sortx[order(sortx)]


xTOTAL<-cbind()

DF<-data.frame(sortx,S2,W2)

sortx<-seq(1:950)
sortx<-100*sortx/950



#Take only number one:
S2sort<-log(S2[order(incsort$totincome10),1:1000])
W2sort<-W2[order(incsort$totincome10),1:1000]
attemptS2<-log(S2[1,1:RR])
attemptW2<-W2[1,1:RR]






#Normalizing
W2sum=matrix(0,950,1)
W2totalitario=W2sort
for (ii in 1:950){
  sumaT=sum(W2sort[ii,1:RR])
  W2totalitario[ii,1:RR]=W2sort[ii,1:RR]/sumaT
}








sortxT<-c(rep(20,RR),rep(40,RR),rep(60,RR),rep(80,RR),rep(100,RR))
S2total<-c(S2sort[82,1:RR],S2sort[203,1:RR],S2sort[469,1:RR],S2sort[686,1:RR],S2sort[717,1:RR])
W2total<-c(W2totalitario[82,1:RR],W2totalitario[203,1:RR],W2totalitario[469,1:RR],W2totalitario[686,1:RR],W2totalitario[717,1:RR])
pscatter<-scatterplot3d(sortxT,S2total,W2total, type = "h", color = "blue",ylim=c(-5,9),angle=40,lab.z=1,
              xlim=c(20,100),xlab="Percentile",ylab="log(skills)",zlab="Density")




#If doing it with png
dev.set()
png('SmoothingdistributionINCOME3.png',
    units="in",
    width=9,
    height=4,
    res=96)
scatterplot3d(sortxT,S2total,W2total, type = "h", color = "blue",ylim=c(-5,9),angle=40,lab.z=1,
              xlim=c(20,100),xlab="Percentile",ylab="log(skills)",zlab="Density")
dev.off()



