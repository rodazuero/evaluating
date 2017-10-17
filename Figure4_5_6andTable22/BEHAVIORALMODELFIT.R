#------------#
#Housekeeping#
#------------#

#source('~/Documents/Research/Chile/RR/BEHAVIORAL71/BEHAVIORALMODELFIT.R', echo=FALSE)

rm(list=ls(all=TRUE))

#Run the necessary functions and libraries
library(ggplot2)
library(gridExtra)
library(xtable)

#Multiplot
source('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/RfunctionsUsed/multiplot.R', echo=TRUE)

#----------------------------------------------------
#Define the parameters of the graphs to be generated#
#----------------------------------------------------
setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/Graphs")

#0. Directory to store and save graphs
initialdir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/Graphs"

#0.1 Directory where the final graphs are going to be saved (combined ones)
finaldir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/Graphs/CombinedResults"

#0.2 Directory where everything is stored
direverything="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71"

#1. Thickness of lines
sizegraphs=2 

#--------------------------#
#Running the rcpp functions#
#--------------------------#


Rcpp::sourceCpp('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/mainRCPP.cpp')
#Rcpp::sourceCpp('/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL63/MainParallel.cpp')

#------------------------------------#
#Loading the optimal parameters found#
#------------------------------------#

#Chose which parameters to load
#BEST FIT: run on the amazonn server

#Initial results
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL64/PAROPTFOUNDPARALLEL2.csv", sep=",", header=FALSE)

#Found in amazon
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL65/PARALLELATTEMPT/amazonserver/PAROPTFOUNDAMAZONfixed.csv", sep=",", header=FALSE)
#Found in tesla
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL65/PARALLELATTEMPT/TESLA/PAROPTFOUNDTESLA.csv", sep=",", header=FALSE)
#PAROPTFOUNDTESLASECONDATTEMPT
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL66/PAROPTFOUNDPARALLELTHIRDFIXED.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL66/PAROPTFOUNDPARALLELFOURTH.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL66/PAROPTFOUNDPARALLELFOURTHFIXED.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL66/PAROPTFOUNDPARALLELFIFTH.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL66/PAROPTFOUNDPARALLELCOMPUTER.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL67/PAROPTFOUNDPARALLELFIFTHFIXED.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL67/PAROPTFOUNDPARALLELSIXTH.csv", sep=",", header=FALSE)

#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL67/PAROPTFOUNDPARALLELSEVENTH.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Documents/Research/Chile/RR/BEHAVIORAL67/PAROPTFOUNDPARALLELEIGHT.csv", sep=",", header=FALSE)

#LATEST FOUND IN OPENMP
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAROPTFOUNDPARALLELSEVENTEENFIXED.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAROPTFOUNDPARALLELNINETEENTHFIXED.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAROPTFOUNDPARALLELTWENTIETH.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAR24.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAR27.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAR28.csv", sep=",", header=FALSE)

algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PARTESLAFOUND.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PARINTERMEDIATE.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/PAR34.csv", sep=",", header=FALSE)

#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL69/PAROPTFOUNDPARALLELSEVENTEENFIXED.csv", sep=",", header=FALSE)
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/PAROPTFOUNDPARALLELTWENTIETH.csv", sep=",", header=FALSE)
#LATEST FOUND IN CUDA



paropt<-algo[,1]

#-----------------------------#
#Loading the necessary dataset#
#-----------------------------#

e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/BEHAVIORALWNAMES.csv", quote="")
data.Hchildcareobs=data.matrix(e$Hchildcareobs,rownames.force=NA)
data.Cchildcare12=data.matrix(e$Cchildcare12,rownames.force=NA)
data.Cchildcare10=data.matrix(e$Cchildcare10,rownames.force=NA)
data.CfactorInv12=data.matrix(e$CfactorInv12,rownames.force=NA)
data.CfactorInv10=data.matrix(e$CfactorInv10,rownames.force=NA)
data.Feffort12=data.matrix(e$Feffort12,rownames.force=NA)
data.Meffort12=data.matrix(e$Meffort12,rownames.force=NA)
data.Feffort10=data.matrix(e$Feffort10,rownames.force=NA)
data.Meffort10=data.matrix(e$Meffort10,rownames.force=NA)
data.Ccareskills=data.matrix(e$Ccareskills,rownames.force=NA)
data.Cedad_meses12=data.matrix(e$Cedad_meses12,rownames.force=NA)
data.Cedad_meses10=data.matrix(e$Cedad_meses10,rownames.force=NA)
data.Cfactorbirth=data.matrix(e$Cfactorbirth,rownames.force=NA)
data.Mwage12=data.matrix(e$Mwage12,rownames.force=NA)
data.Fwage12=data.matrix(e$Fwage12,rownames.force=NA)
data.Mwage10=data.matrix(e$Mwage10,rownames.force=NA)
data.Fwage10=data.matrix(e$Fwage10,rownames.force=NA)
data.Mnlincome12=data.matrix(e$Mnlincome12,rownames.force=NA)
data.Fnlincome12=data.matrix(e$Fnlincome12,rownames.force=NA)
data.Mnlincome10=data.matrix(e$Mnlincome10,rownames.force=NA)
data.Fnlincome10=data.matrix(e$Fnlincome10,rownames.force=NA)
data.Myrschool12=data.matrix(e$Myrschool12,rownames.force=NA)
data.Fyrschool12=data.matrix(e$Fyrschool12,rownames.force=NA)
data.Mage12=data.matrix(e$Mage12,rownames.force=NA)
data.Fage12=data.matrix(e$Fage12,rownames.force=NA)
data.Mage10=data.matrix(e$Mage10,rownames.force=NA)
data.Fage10=data.matrix(e$Fage10,rownames.force=NA)
data.Ffraclabor12=data.matrix(e$Ffraclabor12,rownames.force=NA)
data.Mfraclabor12=data.matrix(e$Mfraclabor12,rownames.force=NA)
data.Ffraclabor10=data.matrix(e$Ffraclabor10,rownames.force=NA)
data.Mfraclabor10=data.matrix(e$Mfraclabor10,rownames.force=NA)
data.Cliveswithmother12=data.matrix(e$Cliveswithmother12,rownames.force=NA)
data.Cliveswithfather12=data.matrix(e$Cliveswithfather12,rownames.force=NA)
data.Cliveswithmother10=data.matrix(e$Cliveswithmother10,rownames.force=NA)
data.Cliveswithfather10=data.matrix(e$Cliveswithfather10,rownames.force=NA)
data.Hhchores12=data.matrix(e$Hchores12,rownames.force=NA)
data.Hbarg=data.matrix(e$Hbarg,rownames.force=NA)
data.Hbarg4=data.matrix(e$Hbarg4,rownames.force=NA)
data.Ctestsfactor4_2012=data.matrix(e$Ctestsfactor4_2012,rownames.force=NA)
data.Ctestsfactor1_10=data.matrix(e$Ctestsfactor1_10,rownames.force=NA)
data.MTJH=data.MTJH=data.matrix(e$MTJH)
data.FMRATIO=data.matrix(e$FMRATIO)
data.Unemployment=data.matrix(e$Unemployment)
data.Wageratio=data.matrix(e$Wageratio)
data.Distance=data.matrix(e$Distance)
data.Magegroup10=data.matrix(e$Magegroup10)
data.Magegroup12=data.matrix(e$Magegroup12)
data.Hmemberstotal10=data.matrix(e$Hmemberstotal10)
data.Hmemberstotal12=data.matrix(e$Hmemberstotal12)


#Transformations of imported dataset
data.CfactorInv12=exp(data.CfactorInv12)
data.CfactorInv10=exp(data.CfactorInv10)
data.Ctestsfactor4_2012=exp(data.Ctestsfactor4_2012)
data.Ctestsfactor1_10=exp(data.Ctestsfactor1_10)
data.Cfactorbirth =exp(data.Cfactorbirth)


#Measurement system -skills in 2010-
data.CSTDtepsi_pb_coo10=e$CSTDtepsi_pb_coo10
data.CSTDtepsi_pb_len10=e$CSTDtepsi_pb_len10
data.CSTDtepsi_pb_mot10=e$CSTDtepsi_pb_mot10
data.CSTDtvip_pb10=e$CSTDtvip_pb10
data.CSTDcbcl1_pb_110=e$CSTDcbcl1_pb_110
data.CSTDcbcl1_pb_210=e$CSTDcbcl1_pb_210
data.CSTDcbcl1_pb_310=e$CSTDcbcl1_pb_310
data.CSTDcbcl1_pb_410=e$CSTDcbcl1_pb_410
data.CSTDcbcl1_pb_510=e$CSTDcbcl1_pb_510
data.CSTDcbcl1_pb_610=e$CSTDcbcl1_pb_610
data.CSTDcbcl1_pb_710=e$CSTDcbcl1_pb_710
MEASKILLS2010=c(data.CSTDtepsi_pb_coo10,data.CSTDtepsi_pb_len10,data.CSTDtepsi_pb_mot10,data.CSTDtvip_pb10,data.CSTDcbcl1_pb_110,
                data.CSTDcbcl1_pb_210,data.CSTDcbcl1_pb_310, data.CSTDcbcl1_pb_410,data.CSTDcbcl1_pb_510,data.CSTDcbcl1_pb_610,
                data.CSTDcbcl1_pb_710)


#Measurement system -skills in 2012-
data.CSTDtadi_pb_cog12=e$CSTDtadi_pb_cog12
data.CSTDtadi_pb_mot12=e$CSTDtadi_pb_mot12
data.CSTDtadi_pb_len12=e$CSTDtadi_pb_len12
data.CSTDtadi_pb_se12=e$CSTDtadi_pb_se12
data.CSTDbt_112=e$CSTDbt_112
data.CSTDbt_212=e$CSTDbt_212
data.CSTDbt_212=e$CSTDbt_212
data.CSTDbt_312=e$CSTDbt_312
data.CSTDbt_412=e$CSTDbt_412
data.CSTDbt_512=e$CSTDbt_512
data.CSTDbt_t12=e$CSTDbt_t12
data.CSTDhtks_st12=e$CSTDhtks_st12
data.CSTDbdst_st12=e$CSTDbdst_st12
data.STDppvt_t12=e$STDppvt_t12

#Density of preschool services:
data.HDens5=e$HDens5

#------------------------------------------------------------#
#-Definition of parameters with the optimal parameters found-#
#------------------------------------------------------------#

aalpha1m=exp(paropt[1])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
aalpha2m=exp(paropt[2])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
aalpha3mINTER=exp(paropt[3])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
aalpha40m=exp(paropt[4])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
aalpha41m=(exp(paropt[5])/(exp(paropt[5])+1))*aalpha40m
ttheta0=exp(paropt[6])/(exp(paropt[6])+exp(paropt[7])+exp(paropt[8]))
ttheta1=exp(paropt[7])/(exp(paropt[6])+exp(paropt[7])+exp(paropt[8]))
ttheta2=exp(paropt[8])/(exp(paropt[6])+exp(paropt[7])+exp(paropt[8]))
pphi=1-exp(-paropt[9])
stdskills=exp(paropt[10])
ggammaf=exp(paropt[11])/(exp(paropt[11])+exp(paropt[12]))
ggammam=exp(paropt[12])/(exp(paropt[11])+exp(paropt[12]))
bbeta0m=paropt[13]
bbeta1m=paropt[14]
bbeta2m=paropt[15]
bbeta3m=paropt[16]
stdwm=exp(paropt[17])
stdeffort=exp(paropt[18])
stdinv=exp(paropt[19])

ddelta0=paropt[21]
ddelta1=paropt[22]
ddelta2=paropt[23]
ddelta3=paropt[24]
ddelta3_12=paropt[25]
aalpha1f=exp(paropt[26])/(exp(paropt[26])+exp(paropt[27])+exp(paropt[28])+exp(paropt[29]))
aalpha2f=exp(paropt[27])/(exp(paropt[26])+exp(paropt[27])+exp(paropt[28])+exp(paropt[29]))
aalpha3f=exp(paropt[28])/(exp(paropt[26])+exp(paropt[27])+exp(paropt[28])+exp(paropt[29]))
aalpha40f=exp(paropt[29])/(exp(paropt[26])+exp(paropt[27])+exp(paropt[28])+exp(paropt[29]))
aalpha41f=(exp(paropt[30])/(exp(paropt[30])+1))*aalpha40f
bbeta0f=paropt[31]
bbeta1f=paropt[32]
bbeta2f=paropt[33]
bbeta3f=paropt[34]
stdwf=exp(paropt[35])
stdeffat=exp(paropt[36])
llambda0=paropt[37]
llambda1=paropt[38]
llambda2=paropt[39]
llambda3=paropt[40]
llambda4=paropt[41]
stdmmu=exp(paropt[42])
mmuLB=0.2
mmuUB=0.8
MEASSkills=exp(paropt[43])
MEASFeffort=exp(paropt[44])
MEASMeffort=exp(paropt[45])
MEASMMu=exp(paropt[46])
MEASINV=exp(paropt[47])

aalpha1m10=exp(paropt[48])/
  (exp(paropt[48])+exp(paropt[49])+exp(paropt[50])+exp(paropt[51])+exp(paropt[52]))

aalpha2m10=exp(paropt[49])/
  (exp(paropt[48])+exp(paropt[49])+exp(paropt[50])+exp(paropt[51])+exp(paropt[52]))

aalpha3m10INTER=exp(paropt[50])/
  (exp(paropt[48])+exp(paropt[49])+exp(paropt[50])+exp(paropt[51])+exp(paropt[52]))


aalpha40m10=exp(paropt[51])/
  (exp(paropt[48])+exp(paropt[49])+exp(paropt[50])+exp(paropt[51])+exp(paropt[52]))


aalpha5m10=exp(paropt[52])/
  (exp(paropt[48])+exp(paropt[49])+exp(paropt[50])+exp(paropt[51])+exp(paropt[52]))

aalpha41m10=exp(paropt[53])/(1+exp(paropt[53]))*aalpha40m10


aalpha1f10=exp(paropt[54])/
  (exp(paropt[54])+exp(paropt[55])+exp(paropt[56])+exp(paropt[57])+exp(paropt[58]))

aalpha2f10=exp(paropt[55])/
  (exp(paropt[54])+exp(paropt[55])+exp(paropt[56])+exp(paropt[57])+exp(paropt[58]))

aalpha3f10=exp(paropt[56])/
  (exp(paropt[54])+exp(paropt[55])+exp(paropt[56])+exp(paropt[57])+exp(paropt[58]))


aalpha40f10=exp(paropt[57])/
  (exp(paropt[54])+exp(paropt[55])+exp(paropt[56])+exp(paropt[57])+exp(paropt[58]))


aalpha5f10=exp(paropt[58])/
  (exp(paropt[54])+exp(paropt[55])+exp(paropt[56])+exp(paropt[57])+exp(paropt[58]))

aalpha41f10=exp(paropt[59])/(1+exp(paropt[59]))*aalpha40f10

pchildcare0=exp(paropt[60])
pchildcare1=exp(paropt[61])
MshockWA=exp(paropt[62])
MshockNWA=exp(paropt[63])
MshockWNA=exp(paropt[64])
MshockNWNA=exp(paropt[65])
FshockWA=exp(paropt[66])
FshockNWA=exp(paropt[67])
FshockWNA=exp(paropt[68])
FshockNWNA=exp(paropt[69])

llambda5=paropt[70]
llambda6=paropt[71]
llambda7=paropt[72]
llambda8=paropt[73]

aalpha3m10_mtjh=(exp(paropt[74])/(1+exp(paropt[74])))*aalpha3m10INTER
aalpha3m12_mtjh=(exp(paropt[75])/(1+exp(paropt[75])))*aalpha3mINTER

if (paropt[76]>500){
  paropt[76]=500
}

aalpha3mage10=(exp(paropt[76])/(1+exp(paropt[76])))*aalpha3m10INTER
aalpha3mage12=(exp(paropt[77])/(1+exp(paropt[77])))*aalpha3mINTER
ddelta4=paropt[78]

SIZE=length(data.Cliveswithfather12)


#---------------------------------------------#
#Prediction of behavior given parameters found#
#---------------------------------------------#


## Predicting everything given the estimated parameters
predhm12<-rep(0,SIZE)
predhm10<-rep(0,SIZE)
predchildcare12<-rep(0,SIZE)
predchildcare10<-rep(0,SIZE)
Fwagepred10<-rep(0,SIZE)
Fwagepred12<-rep(0,SIZE)
Mwagepred10<-rep(0,SIZE)
Mwagepred12<-rep(0,SIZE)
#-----------
Ffraclabor10PRED<-rep(0,SIZE)
Mfraclabor10PRED<-rep(0,SIZE)
Ffraclabor12PRED<-rep(0,SIZE)
Mfraclabor12PRED<-rep(0,SIZE)
#----------
Ffraclabor10PRED<-rep(0,SIZE)
Mfraclabor10PRED<-rep(0,SIZE)
Ffraclabor12PRED<-rep(0,SIZE)
Mfraclabor12PRED<-rep(0,SIZE)

#Effort levels
Meffort10PRED<-rep(0,SIZE)
Meffort12PRED<-rep(0,SIZE)
Feffort10PRED<-rep(0,SIZE)
Feffort12PRED<-rep(0,SIZE)

#Investment
Cinv10PRED<-rep(0,SIZE)
Cinv12PRED<-rep(0,SIZE)

#Skills
Cskills10PRED<-rep(0,SIZE)
Cskills12PRED<-rep(0,SIZE)


for (ii in 1:SIZE){
  #1. Predicting labor supply: 
  
  
  #Define the aalpha4:
  aalpha4f10=aalpha40f10+aalpha41f10*data.Hhchores12[ii]
  aalpha4m10=aalpha40m10+aalpha41m10*data.Hhchores12[ii]
  
  aalpha4f=aalpha40f+aalpha41f*data.Hhchores12[ii]
  aalpha4m=aalpha40m+aalpha41m*data.Hhchores12[ii]
  
  #Define aalpha3: However, if 
  #going to use OPTIMIZEDPARAMETERS.csv, comment the aalpha3 re-definitions. 
  
  aalpha3m10=aalpha3m10INTER-aalpha3m10_mtjh*data.MTJH[ii]+aalpha3mage10*data.Magegroup10[ii]
  aalpha3m=aalpha3mINTER-aalpha3m12_mtjh*data.MTJH[ii]+aalpha3mage12*data.Magegroup12[ii]
  #Define the price of childcare
  pricechildcare=pchildcare0+(1/(1+data.Hchildcareobs[ii]))*pchildcare1
  price=exp(paropt[20])-exp(paropt[325])*(data.HDens5[ii])
  #1.1 Obtaining wages
  
  #1.1.1 Wages of Father
  #==========================
  if (data.Ffraclabor10[ii]==1){
    Fwagepred10[ii]=data.Fwage10[ii]
  }
  if (data.Ffraclabor10[ii]==0){
    Fwagepred10[ii]=F_predwage(bbeta0f,bbeta1f,bbeta2f,bbeta3f,data.Fyrschool12[ii],data.Fage10[ii])
  }
  
  if (data.Ffraclabor12[ii]==1){
    Fwagepred12[ii]=data.Fwage12[ii]
  }
  if (data.Ffraclabor12[ii]==0){
    Fwagepred12[ii]=F_predwage(bbeta0f,bbeta1f,bbeta2f,bbeta3f,data.Fyrschool12[ii],data.Fage12[ii])
  }
  #1.1.2 Wages of mother
  #===============================
  if (data.Mfraclabor10[ii]==1){
    Mwagepred10[ii]=data.Mwage10[ii]
  }
  if (data.Mfraclabor10[ii]==0){
    Mwagepred10[ii]=F_predwage(bbeta0m,bbeta1m,bbeta2m,bbeta3m,data.Myrschool12[ii],data.Mage10[ii])
  }
  
  if (data.Mfraclabor12[ii]==1){
    Mwagepred12[ii]=data.Mwage12[ii]
  }
  if (data.Mfraclabor12[ii]==0){
    Mwagepred12[ii]=F_predwage(bbeta0m,bbeta1m,bbeta2m,bbeta3m,data.Myrschool12[ii],data.Mage12[ii])
  }
  
  
  #=================================================
  #1.3. Obtaining utilities depending on each case. 
  #=================================================
  
  #1.3.1 First if they live together
  #-------------------------------
  if ( (data.Cliveswithfather10[ii]==1)  && (data.Cliveswithmother10[ii]==1)){
    #0. Getting the predicted level of mmu:
    mmu=F_mmu(llambda0,llambda1,llambda2,llambda3,llambda4,llambda5,llambda6,llambda7,llambda8,Fwagepred10[ii],
              Mwagepred10[ii],data.Fnlincome10[ii],data.Mnlincome10[ii],0,
              0.2,0.8,data.Fage12[ii],data.Mage12[ii],data.Fyrschool12[ii],data.Myrschool12[ii],data.FMRATIO[ii],data.Unemployment[ii],data.Wageratio[ii],data.Distance[ii])
    
    #1. Predicted effort levels
    MeffortNWNW_TOG10=F_effort_m10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                   aalpha4f10,aalpha4m10,0,0,pphi,ttheta0,ttheta2,0.92)
    MeffortNWW_TOG10=F_effort_m10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                  aalpha4f10,aalpha4m10,0,1,pphi,ttheta0,ttheta2,0.92)
    MeffortWNW_TOG10=F_effort_m10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                  aalpha4f10,aalpha4m10,1,0,pphi,ttheta0,ttheta2,0.92)
    MeffortWW_TOG10=F_effort_m10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                 aalpha4f10,aalpha4m10,1,1,pphi,ttheta0,ttheta2,0.92)
    
    FeffortNWNW_TOG10=F_effort_f10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                   aalpha4f10,aalpha4m10,0,0,pphi,ttheta0,ttheta2,0.92)
    FeffortNWW_TOG10=F_effort_f10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                  aalpha4f10,aalpha4m10,0,1,pphi,ttheta0,ttheta2,0.92)
    FeffortWNW_TOG10=F_effort_f10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                  aalpha4f10,aalpha4m10,1,0,pphi,ttheta0,ttheta2,0.92)
    FeffortWW_TOG10=F_effort_f10(mmu,ggammaf,aalpha2f10,aalpha2m10,
                                 aalpha4f10,aalpha4m10,1,1,pphi,ttheta0,ttheta2,0.92)
    
    #2. Predicted levels of investment
    CinvestmentNWNWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                          aalpha2f10,aalpha2m10,mmu,0,0,Fwagepred10[ii],Mwagepred10[ii],
                                          data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,price)
    
    CinvestmentNWNWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                         aalpha2f10,aalpha2m10,mmu,0,0,Fwagepred10[ii],Mwagepred10[ii],
                                         data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,price)
    
    CinvestmentNWWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                         aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                         data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,price)
    
    CinvestmentNWWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                        aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                        data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,price)
    
    CinvestmentNWWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                         aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                         data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,price)
    
    CinvestmentNWWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                        aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                        data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,price)
    
    CinvestmentWNWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                         aalpha2f10,aalpha2m10,mmu,1,0,Fwagepred10[ii],Mwagepred10[ii],
                                         data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,price)
    
    CinvestmentWNWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                        aalpha2f10,aalpha2m10,mmu,1,0,Fwagepred10[ii],Mwagepred10[ii],
                                        data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,price)
    
    
    CinvestmentWWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                        aalpha2f10,aalpha2m10,mmu,1,1,Fwagepred10[ii],Mwagepred10[ii],
                                        data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,price)
    
    CinvestmentWWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                       aalpha2f10,aalpha2m10,mmu,1,1,Fwagepred10[ii],Mwagepred10[ii],
                                       data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,price)
    
    
    #3. Predicted levels of skills
    CSkillsNWNWNA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                     ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWNW_TOG10,MeffortNWNW_TOG10,
                                     CinvestmentNWNWNA_TOG10,data.Cfactorbirth[ii],0,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    CSkillsNWNWA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWNW_TOG10,MeffortNWNW_TOG10,
                                    CinvestmentNWNWA_TOG10,data.Cfactorbirth[ii],1,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    CSkillsWNWNA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG10,MeffortWNW_TOG10,
                                    CinvestmentWNWNA_TOG10,data.Cfactorbirth[ii],0,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    CSkillsWNWA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                   ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG10,MeffortWNW_TOG10,
                                   CinvestmentWNWA_TOG10,data.Cfactorbirth[ii],1,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    
    CSkillsNWWNA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG10,MeffortNWW_TOG10,
                                    CinvestmentNWWNA_TOG10,data.Cfactorbirth[ii],0,data.Ccareskills[ii],data.Hmemberstotal10[ii]) 
    
    CSkillsNWWA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                   ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG10,MeffortNWW_TOG10,
                                   CinvestmentNWWA_TOG10,data.Cfactorbirth[ii],1,data.Ccareskills[ii],data.Hmemberstotal10[ii]) 
    
    
    CSkillsWWNA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                   ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG10,MeffortWW_TOG10,
                                   CinvestmentWWNA_TOG10,data.Cfactorbirth[ii],0,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    CSkillsWWA_TOG10=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,data.Cedad_meses10[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG10,MeffortWW_TOG10,
                                  CinvestmentWWA_TOG10,data.Cfactorbirth[ii],1,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    #4. Getting consumption levels
    FConsumptionNWNWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentNWNWNA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionNWNWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentNWNWNA_TOG10,ttheta0,ttheta1,mmu,price)
    
    FConsumptionNWNWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentNWNWA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionNWNWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentNWNWA_TOG10,ttheta0,ttheta1,mmu,price)
    
    
    FConsumptionWNWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentWNWNA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionWNWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentWNWNA_TOG10,ttheta0,ttheta1,mmu,price)
    
    FConsumptionWNWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                               aalpha2m10,CinvestmentWNWA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionWNWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                               aalpha2m10,CinvestmentWNWA_TOG10,ttheta0,ttheta1,mmu,price)
    
    
    
    
    FConsumptionNWWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentNWWNA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionNWWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentNWWNA_TOG10,ttheta0,ttheta1,mmu,price)
    
    FConsumptionNWWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                               aalpha2m10,CinvestmentNWWA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionNWWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                               aalpha2m10,CinvestmentNWWA_TOG10,ttheta0,ttheta1,mmu,price)
    
    
    FConsumptionWWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                               aalpha2m10,CinvestmentWWNA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionWWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                               aalpha2m10,CinvestmentWWNA_TOG10,ttheta0,ttheta1,mmu,price)
    
    FConsumptionWWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                              aalpha2m10,CinvestmentWWA_TOG10,ttheta0,ttheta1,mmu,price)
    MConsumptionWWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                              aalpha2m10,CinvestmentWWA_TOG10,ttheta0,ttheta1,mmu,price)
    
    #5. Utility levels
    FutilityNWNWNA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                     aalpha5f10,FConsumptionNWNWNA_TOG10, FeffortNWNW_TOG10,0,
                                     CSkillsNWNWNA_TOG10,0)
    
    MutilityNWNWNA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                     aalpha5m10,MConsumptionNWNWNA_TOG10, MeffortNWNW_TOG10,0,
                                     CSkillsNWNWNA_TOG10,0)
    #-----------
    
    FutilityNWNWA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                    aalpha5f10,FConsumptionNWNWA_TOG10, FeffortNWNW_TOG10,0,
                                    CSkillsNWNWA_TOG10,1)
    
    MutilityNWNWA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                    aalpha5m10,MConsumptionNWNWA_TOG10, MeffortNWNW_TOG10,0,
                                    CSkillsNWNWA_TOG10,1)
    
    #----------------
    
    FutilityWNWNA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                    aalpha5f10,FConsumptionWNWNA_TOG10, FeffortWNW_TOG10,1,
                                    CSkillsWNWNA_TOG10,0)
    
    MutilityWNWNA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                    aalpha5m10,MConsumptionWNWNA_TOG10, MeffortWNW_TOG10,0,
                                    CSkillsWNWNA_TOG10,0)
    
    #----------------------
    
    FutilityWNWA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                   aalpha5f10,FConsumptionWNWA_TOG10, FeffortWNW_TOG10,1,
                                   CSkillsWNWA_TOG10,1)
    
    MutilityWNWA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                   aalpha5m10,MConsumptionWNWA_TOG10, MeffortWNW_TOG10,0,
                                   CSkillsWNWA_TOG10,1)
    
    #-------------------------
    
    FutilityNWWNA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                    aalpha5f10,FConsumptionNWWNA_TOG10, FeffortNWW_TOG10,0,
                                    CSkillsNWWNA_TOG10,0)
    
    MutilityNWWNA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                    aalpha5m10,MConsumptionNWWNA_TOG10, MeffortNWW_TOG10,1,
                                    CSkillsNWWNA_TOG10,0)
    
    #--------------------------
    
    
    FutilityNWWA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                   aalpha5f10,FConsumptionNWWA_TOG10, FeffortNWW_TOG10,0,
                                   CSkillsNWWA_TOG10,1)
    
    MutilityNWWA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                   aalpha5m10,MConsumptionNWWA_TOG10, MeffortNWW_TOG10,1,
                                   CSkillsNWWA_TOG10,1)
    
    #-------------------------
    
    FutilityWWNA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                   aalpha5f10,FConsumptionWWNA_TOG10, FeffortWW_TOG10,1,
                                   CSkillsWWNA_TOG10,0)
    
    MutilityWWNA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                   aalpha5m10,MConsumptionWWNA_TOG10, MeffortWW_TOG10,1,
                                   CSkillsWWNA_TOG10,0)
    
    
    #-------------------------
    
    FutilityWWA_TOG10=F_utility10(aalpha1f10,aalpha2f10,aalpha3f10,aalpha4f10,
                                  aalpha5f10,FConsumptionWWA_TOG10, FeffortWW_TOG10,1,
                                  CSkillsWWA_TOG10,1)
    
    MutilityWWA_TOG10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,
                                  aalpha5m10,MConsumptionWWA_TOG10, MeffortWW_TOG10,1,
                                  CSkillsWWA_TOG10,1)
    
    
    #6. Welfare
    WelfareNWNWNA10=mmu*FutilityNWNWNA_TOG10+(1-mmu)*MutilityNWNWNA_TOG10
    WelfareNWNWA10=mmu*FutilityNWNWA_TOG10+(1-mmu)*MutilityNWNWA_TOG10
    #-
    WelfareWNWNA10=mmu*FutilityWNWNA_TOG10+(1-mmu)*MutilityWNWNA_TOG10
    WelfareWNWA10=mmu*FutilityWNWA_TOG10+(1-mmu)*MutilityWNWA_TOG10
    #-
    WelfareNWWNA10=mmu*FutilityNWWNA_TOG10+(1-mmu)*MutilityNWWNA_TOG10
    WelfareNWWA10=mmu*FutilityNWWA_TOG10+(1-mmu)*MutilityNWWA_TOG10
    #-
    WelfareWWNA10=mmu*FutilityWWNA_TOG10+(1-mmu)*MutilityWWNA_TOG10
    WelfareWWA10=mmu*FutilityWWA_TOG10+(1-mmu)*MutilityWWA_TOG10
    
    #7. Identifying the decision taken in 2010:
    maxWELF10=max(c(WelfareNWNWNA10,
                    WelfareNWNWA10,
                    WelfareWNWNA10,
                    WelfareWNWA10,
                    WelfareNWWNA10,
                    WelfareNWWA10,
                    WelfareWWNA10,
                    WelfareWWA10))
    
    #If predicted is NWNWNA
    if (maxWELF10==WelfareNWNWNA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortNWNW_TOG10
      Feffort10PRED[ii]=FeffortNWNW_TOG10
      Cinv10PRED[ii]=CinvestmentNWNWNA_TOG10
      Cskills10PRED[ii]=CSkillsNWNWNA_TOG10
    }
    
    #If predicted is NWNWA
    if (maxWELF10==WelfareNWNWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortNWNW_TOG10
      Feffort10PRED[ii]=FeffortNWNW_TOG10
      Cinv10PRED[ii]=CinvestmentNWNWA_TOG10
      Cskills10PRED[ii]=CSkillsNWNWA_TOG10
    }
    
    
    #If predicted is WNWNA
    if (maxWELF10==WelfareWNWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWNA_TOG10
      Cskills10PRED[ii]=CSkillsWNWNA_TOG10
    }
    
    #If predicted is WNWA
    if (maxWELF10==WelfareWNWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWA_TOG10
      Cskills10PRED[ii]=CSkillsWNWA_TOG10
    }
    
    #If predicted is NWWNA
    if (maxWELF10==WelfareNWWNA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWNA_TOG10
      Cskills10PRED[ii]=CSkillsNWWNA_TOG10
    }
    
    #If predicted is NWWA
    if (maxWELF10==WelfareNWWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWA_TOG10
      Cskills10PRED[ii]=CSkillsNWWA_TOG10
    }
    
    
    #If predicted is WWNA
    if (maxWELF10==WelfareWWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWNA_TOG10
      Cskills10PRED[ii]=CSkillsWWNA_TOG10
    }
    
    #If predicted is WWA
    if (maxWELF10==WelfareWWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWA_TOG10
      Cskills10PRED[ii]=CSkillsWWA_TOG10
    }
    
  }#End if they live together
  
  #If single mother in 2010:
  if (data.Cliveswithmother10[ii]==1 & data.Cliveswithfather10[ii]==0){
    #1. Effort predicted
    Meffort10NW=Meffortsolver10(aalpha2m,aalpha2m10,aalpha4m10,ggammaf,
                                data.Feffort10[ii],pphi,ttheta0,ttheta2,0)
    Meffort10W=Meffortsolver10(aalpha2m,aalpha2m10,aalpha4m10,ggammaf,
                               data.Feffort10[ii],pphi,ttheta0,ttheta2,1)
    #2. Investment levels
    
    Cinvestment10NWNA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                     Mwagepred10[ii],data.Mnlincome10[ii],0,price)
    
    Cinvestment10NWA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                    Mwagepred10[ii],data.Mnlincome10[ii]-pricechildcare,0,price)
    
    
    Cinvestment10WNA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                    Mwagepred10[ii],data.Mnlincome10[ii],1,price)
    
    Cinvestment10WA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                   Mwagepred10[ii],data.Mnlincome10[ii]-pricechildcare,1,price)
    
    #3. Skills
    CSkills10NWNA=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,
                               data.Cedad_meses10[ii],ttheta0,ttheta1,
                               ttheta2,pphi,ggammaf,ggammam,data.Feffort10[ii],Meffort10NW,Cinvestment10NWNA,
                               data.Cfactorbirth[ii],0,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    CSkills10NWA=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,
                              data.Cedad_meses10[ii],ttheta0,ttheta1,
                              ttheta2,pphi,ggammaf,ggammam,data.Feffort10[ii],Meffort10NW,Cinvestment10NWA,
                              data.Cfactorbirth[ii],1,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    
    
    CSkills10WNA=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,
                              data.Cedad_meses10[ii],ttheta0,ttheta1,
                              ttheta2,pphi,ggammaf,ggammam,data.Feffort10[ii],Meffort10NW,Cinvestment10NWA,
                              data.Cfactorbirth[ii],0,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    CSkills10WA=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3,ddelta4,
                             data.Cedad_meses10[ii],ttheta0,ttheta1,
                             ttheta2,pphi,ggammaf,ggammam,data.Feffort10[ii],Meffort10NW,Cinvestment10WA,
                             data.Cfactorbirth[ii],1,data.Ccareskills[ii],data.Hmemberstotal10[ii])
    
    #4. Consumption
    MconsumptionNWNA10=F_consumption(aalpha1m10,aalpha2m10,ttheta1,Mwagepred10[ii],
                                     data.Mnlincome10[ii],0)
    MconsumptionNWA10=F_consumption(aalpha1m10,aalpha2m10,ttheta1,Mwagepred10[ii],
                                    data.Mnlincome10[ii]-pricechildcare,0)
    
    MconsumptionWNA10=F_consumption(aalpha1m10,aalpha2m10,ttheta1,Mwagepred10[ii],
                                    data.Mnlincome10[ii],1)
    MconsumptionWA10=F_consumption(aalpha1m10,aalpha2m10,ttheta1,Mwagepred10[ii],
                                   data.Mnlincome10[ii]-pricechildcare,1)
    
    #5. Utility
    MutilityNWNA10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,aalpha5m10,
                               MconsumptionNWNA10,Meffort10NW,0,CSkills10NWNA,0)
    MutilityNWA10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,aalpha5m10,
                              MconsumptionNWA10,Meffort10NW,0,CSkills10NWA,1)
    
    MutilityWNA10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,aalpha5m10,
                              MconsumptionWNA10,Meffort10W,0,CSkills10NWNA,0)
    MutilityWA10=F_utility10(aalpha1m10,aalpha2m10,aalpha3m10,aalpha4m10,aalpha5m10,
                             MconsumptionWA10,Meffort10W,1,CSkills10WA,1)
    #6. Identifying the best decision possible
    maxWELF10=max(c(MutilityNWNA10,MutilityNWA10,MutilityWNA10,MutilityWA10))
    
    #6. Identifying the decision
    if ((maxWELF10==MutilityNWNA10)){
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=0
      Meffort10PRED[ii]=Meffort10NW
      Cinv10PRED[ii]=Cinvestment10NWNA
      Cskills10PRED[ii]=CSkills10NWNA
    }else if ((maxWELF10==MutilityNWA10) ){
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=Meffort10NW
      Cinv10PRED[ii]=Cinvestment10NWA
      Cskills10PRED[ii]=CSkills10NWA
    }else if ((maxWELF10==MutilityWNA10) ){
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=Meffort10W
      Cinv10PRED[ii]=Cinvestment10WNA
      Cskills10PRED[ii]=CSkills10WNA
    }else if ((maxWELF10==MutilityWA10) ){
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=Meffort10W
      Cinv10PRED[ii]=Cinvestment10WA
      Cskills10PRED[ii]=CSkills10WA
    }
    
    
    
  }#End if single mother in 2010
  
  
  #Now, in the 2012
  #1. If they live together
  if ( (data.Cliveswithfather12[ii]==1)  && (data.Cliveswithmother12[ii]==1)){
    #0. Getting the predicted level of mmu:
    mmu12=F_mmu(llambda0,llambda1,llambda2,llambda3,llambda4,llambda5,llambda6,llambda7,llambda8,Fwagepred12[ii],
                Mwagepred12[ii],data.Fnlincome12[ii],data.Mnlincome12[ii],0,
                0.2,0.8,data.Fage12[ii],data.Mage12[ii],data.Fyrschool12[ii],data.Myrschool12[ii],data.FMRATIO[ii],
                data.Unemployment[ii],data.Wageratio[ii],data.Distance[ii])
    
    #1. Predicted effort levels
    MeffortNWNW_TOG12=F_effort_m(mmu12,ggammaf,aalpha2f,aalpha2m,
                                 aalpha4f,aalpha4m,0,0,pphi,ttheta2)
    MeffortNWW_TOG12=F_effort_m(mmu12,ggammaf,aalpha2f,aalpha2m,
                                aalpha4f,aalpha4m,0,1,pphi,ttheta2)
    MeffortWNW_TOG12=F_effort_f(mmu12,ggammaf,aalpha2f,aalpha2m,
                                aalpha4f,aalpha4m,1,0,pphi,ttheta2)
    MeffortWW_TOG12=F_effort_m(mmu12,ggammaf,aalpha2f,aalpha2m,
                               aalpha4f,aalpha4m,1,1,pphi,ttheta2)
    
    FeffortNWNW_TOG12=F_effort_f(mmu12,ggammaf,aalpha2f,aalpha2m,
                                 aalpha4f,aalpha4m,0,0,pphi,ttheta2)
    FeffortNWW_TOG12=F_effort_f(mmu12,ggammaf,aalpha2f,aalpha2m,
                                aalpha4f,aalpha4m,0,1,pphi,ttheta2)
    FeffortWNW_TOG12=F_effort_f(mmu12,ggammaf,aalpha2f,aalpha2m,
                                aalpha4f,aalpha4m,1,0,pphi,ttheta2)
    FeffortWW_TOG12=F_effort_f(mmu12,ggammaf,aalpha2f,aalpha2m,
                               aalpha4f,aalpha4m,1,1,pphi,ttheta2)
    
    #2. Predicted levels of investment
    CinvestmentNWNW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                      mmu12,0,0,Fwagepred12[ii],Mwagepred12[ii],
                                      data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,price)
    
    CinvestmentWNW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                     mmu12,1,0,Fwagepred12[ii],Mwagepred12[ii],
                                     data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,price)
    CinvestmentNWW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                     mmu12,0,1,Fwagepred12[ii],Mwagepred12[ii],
                                     data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,price)
    CinvestmentWW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                    mmu12,1,1,Fwagepred12[ii],Mwagepred12[ii],
                                    data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,price)
    
    
    #3. Predicted levels of skills
    CSkillsNWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                   ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWNW_TOG12,MeffortNWNW_TOG12,
                                   CinvestmentNWNW_TOG12,data.Ctestsfactor1_10[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG12,MeffortWNW_TOG12,
                                  CinvestmentWNW_TOG12,data.Ctestsfactor1_10[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsNWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG12,MeffortNWW_TOG12,
                                  CinvestmentNWW_TOG12,data.Ctestsfactor1_10[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                 ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG12,MeffortWW_TOG12,
                                 CinvestmentWW_TOG12,data.Ctestsfactor1_10[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    #4. Getting consumption levels
    FConsumptionNWNW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                             CinvestmentNWNW_TOG12,ttheta1,mmu12,price)
    MConsumptionNWNW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                             CinvestmentNWNW_TOG12,ttheta1,mmu12,price)
    
    FConsumptionWNW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                            CinvestmentWNW_TOG12,ttheta1,mmu12,price)
    MConsumptionWNW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                            CinvestmentWNW_TOG12,ttheta1,mmu12,price)
    
    FConsumptionNWW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                            CinvestmentNWW_TOG12,ttheta1,mmu12,price)
    MConsumptionNWW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                            CinvestmentNWW_TOG12,ttheta1,mmu12,price)
    
    FConsumptionWW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                           CinvestmentWW_TOG12,ttheta1,mmu12,price)
    MConsumptionWW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                           CinvestmentWW_TOG12,ttheta1,mmu12,price)
    
    #5. Utility levels
    FutilityNWNW_TOG12=F_utility(aalpha1f,aalpha2f,aalpha3f,aalpha4f,
                                 FConsumptionNWNW_TOG12, FeffortNWNW_TOG12,0,
                                 CSkillsNWNW_TOG12)
    
    MutilityNWNW_TOG12=F_utility(aalpha1m,aalpha2m,aalpha3m,aalpha4m,
                                 MConsumptionNWNW_TOG12, MeffortNWNW_TOG12,0,
                                 CSkillsNWNW_TOG12)
    
    #--------
    
    FutilityWNW_TOG12=F_utility(aalpha1f,aalpha2f,aalpha3f,aalpha4f,
                                FConsumptionWNW_TOG12, FeffortWNW_TOG12,1,
                                CSkillsWNW_TOG12)
    
    MutilityWNW_TOG12=F_utility(aalpha1m,aalpha2m,aalpha3m,aalpha4m,
                                MConsumptionWNW_TOG12, MeffortWNW_TOG12,0,
                                CSkillsWNW_TOG12)
    
    #--------
    
    FutilityNWW_TOG12=F_utility(aalpha1f,aalpha2f,aalpha3f,aalpha4f,
                                FConsumptionNWW_TOG12, FeffortNWW_TOG12,0,
                                CSkillsNWW_TOG12)
    
    MutilityNWW_TOG12=F_utility(aalpha1m,aalpha2m,aalpha3m,aalpha4m,
                                MConsumptionNWW_TOG12, MeffortNWW_TOG12,1,
                                CSkillsNWW_TOG12)
    
    #--------
    
    FutilityWW_TOG12=F_utility(aalpha1f,aalpha2f,aalpha3f,aalpha4f,
                               FConsumptionWW_TOG12, FeffortWW_TOG12,1,
                               CSkillsWW_TOG12)
    
    MutilityWW_TOG12=F_utility(aalpha1m,aalpha2m,aalpha3m,aalpha4m,
                               MConsumptionWW_TOG12, MeffortWW_TOG12,1,
                               CSkillsWW_TOG12)
    
    #--------
    
    #6. Welfare
    WelfareNWNW12=mmu12*FutilityNWNW_TOG12+(1-mmu12)*MutilityNWNW_TOG12
    WelfareWNW12=mmu12*FutilityWNW_TOG12+(1-mmu12)*MutilityWNW_TOG12
    WelfareNWW12=mmu12*FutilityNWW_TOG12+(1-mmu12)*MutilityNWW_TOG12
    WelfareWW12=mmu12*FutilityWW_TOG12+(1-mmu12)*MutilityWW_TOG12
    
    #7. Identifying the decision taken in 2010:
    maxWELF12=max(c(WelfareNWNW12,WelfareWNW12,WelfareNWW12,WelfareWW12))
    if (maxWELF12==WelfareNWNW12){
      Ffraclabor12PRED[ii]=0
      Mfraclabor12PRED[ii]=0
      Meffort12PRED[ii]=MeffortNWNW_TOG12
      Feffort12PRED[ii]=FeffortNWNW_TOG12
      Cinv12PRED[ii]=CinvestmentNWNW_TOG12
      Cskills12PRED[ii]=CSkillsNWNW_TOG12
    }
    if (maxWELF12==WelfareWNW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=0
      Meffort12PRED[ii]=MeffortWNW_TOG12
      Feffort12PRED[ii]=FeffortWNW_TOG12
      Cinv12PRED[ii]=CinvestmentWNW_TOG12
      Cskills12PRED[ii]=CSkillsWNW_TOG12
    }
    if (maxWELF12==WelfareNWW12){
      Ffraclabor12PRED[ii]=0
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortNWW_TOG12
      Feffort12PRED[ii]=FeffortNWW_TOG12
      Cinv12PRED[ii]=CinvestmentNWW_TOG12
      Cskills12PRED[ii]=CSkillsNWW_TOG12
    }
    if (maxWELF12==WelfareWW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortWW_TOG12
      Feffort12PRED[ii]=FeffortWW_TOG12
      Cinv12PRED[ii]=CinvestmentWW_TOG12
      Cskills12PRED[ii]=CSkillsWW_TOG12
    }
    
  }#End if they live together 2012
  
  #Single mother in 2012:
  if (data.Cliveswithmother12[ii]==1 & data.Cliveswithfather12[ii]==0){
    #1. Effort predicted
    Meffort12NW=Meffortsolver(aalpha2m,aalpha4m,ggammaf,
                              data.Feffort12[ii],pphi,ttheta2,0)
    Meffort12W=Meffortsolver(aalpha2m,aalpha4m,ggammaf,
                             data.Feffort12[ii],pphi,ttheta2,1)
    #2. Investment levels
    Cinvestment12NW=F_investment(aalpha1m,aalpha2m,ttheta1,
                                 Mwagepred12[ii],data.Mnlincome12[ii],0,price)
    Cinvestment12W=F_investment(aalpha1m,aalpha2m,ttheta1,
                                Mwagepred12[ii],data.Mnlincome12[ii],1,price)
    #3. Skills
    CSkills12NW=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,ttheta1,
                             ttheta2,pphi,ggammaf,ggammam,data.Feffort12[ii],Meffort12NW,Cinvestment12NW,
                             data.Ctestsfactor1_10[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    
    CSkills12W=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses10[ii],ttheta0,ttheta1,
                            ttheta2,pphi,ggammaf,ggammam,data.Feffort10[ii],Meffort12W,Cinvestment12W,
                            data.Ctestsfactor1_10[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    
    #4. Consumption
    MconsumptionW12=F_consumption(aalpha1m,aalpha2m,ttheta1,Mwagepred12[ii],
                                  data.Mnlincome12[ii],1)
    MconsumptionNW12=F_consumption(aalpha1m,aalpha2m,ttheta1,Mwagepred12[ii],
                                   data.Mnlincome12[ii],0)
    #5. Utility
    MutilityW12=F_utility(aalpha1m,aalpha2m,aalpha3m,aalpha4m,
                          MconsumptionW12,Meffort12W,1,CSkills12W)
    MutilityNW12=F_utility(aalpha1m,aalpha2m,aalpha3m,aalpha4m,
                           MconsumptionNW12,Meffort12NW,0,CSkills12NW)
    #6. Identifying the decision
    if ((MutilityNW12>MutilityW12)){
      Mfraclabor12PRED[ii]=0
      Meffort12PRED[ii]=Meffort12NW
      Cinv12PRED[ii]=Cinvestment12NW
      Cskills12PRED[ii]=CSkills12NW
    }else {
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=Meffort12W
      Cinv12PRED[ii]=Cinvestment12W
      Cskills12PRED[ii]=CSkills12W
    }
    
  }#End if single mother in 2012
  
}


#Generate dataframe
PREDFINAL<-data.frame(Mfraclabor12PRED,Ffraclabor12PRED,
                      data.Mage12,data.Fage12,
                      Mfraclabor10PRED,Ffraclabor10PRED,
                      data.Mfraclabor12,data.Ffraclabor12,
                      data.Mfraclabor10,data.Ffraclabor10,
                      data.Myrschool12,data.Fyrschool12,
                      data.Cliveswithfather12,data.Cliveswithmother12,
                      data.Cliveswithfather10,data.Cliveswithmother10,
                      data.Cchildcare10,predchildcare10,
                      data.Cedad_meses10,data.Mage10,data.Fage10)


#Variance of skills
sqrt(var(log(Cskills12PRED)))
#----------------------------------------------------
#I. Model fit ---- MARRIED WOMEN
#----------------------------------------------------


PREDFINALMARR<-subset(PREDFINAL, data.Cliveswithfather12==1 & 
                        data.Cliveswithmother12==1)

#Labor market decisions By individual age group
MINAGE=19
MAXAGE=49
RANK=MAXAGE-MINAGE
seq.age<-c(seq(MINAGE,2*RANK,1))
seq.Mfraclabor12<-0*seq(MINAGE,2*RANK,1)
seq.Mfraclabor10<-0*seq(MINAGE,2*RANK,1)
seq.Cchildcare10<-0*seq(MINAGE,2*RANK,1)
seq.Ffraclabor12<-0*seq(MINAGE,2*RANK,1)
seq.Ffraclabor10<-0*seq(MINAGE,2*RANK,1)
seq.indpred<-0*seq(MINAGE,2*RANK,1)

for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Mage12==yy)
  #2012
  seq.Mfraclabor12[tt]<-mean(temp$data.Mfraclabor12)
  seq.Mfraclabor12[ttpred]<-mean(temp$Mfraclabor12PRED)
  #2010
  temp<-subset(PREDFINALMARR, data.Mage10==yy)
  seq.Mfraclabor10[tt]<-mean(temp$data.Mfraclabor10)
  seq.Mfraclabor10[ttpred]<-mean(temp$Mfraclabor10PRED)
  #Extracting father's mean
  temp<-subset(PREDFINALMARR,data.Fage12==yy)
  seq.Ffraclabor12[tt]<-mean(temp$data.Ffraclabor12)
  seq.Ffraclabor12[ttpred]<-mean(temp$Ffraclabor12PRED)
  
  temp<-subset(PREDFINALMARR, data.Fage10==yy)
  seq.Ffraclabor10[tt]<-mean(temp$data.Ffraclabor10)
  seq.Ffraclabor10[ttpred]<-mean(temp$Ffraclabor10PRED)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}
seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Mfraclabor12,seq.Ffraclabor12,seq.Mfraclabor10,seq.Ffraclabor10,seq.indpred,seq.age)

#--------------------#
#Plotting the results#
#--------------------#

#The vector labormothers will contain the name of graphs related to fitting labor mothers
#The vector laborfathers will contain the name of the graphs related to fitting labor fathers


#---------------
#By age individual
#----2012-----#
xl12<-xlim(21,44)
xl10<-xlim(19,42)

#1.1 Lines for mothers 
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Mfraclabor12, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+labs(title="2012", x="Age", y="Proportion working")
p<-p+ylim(0,1)
#p<-p+xl12
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Mothermarriedageline.pdf')
pline
dev.off()
marriedmothers2012labor<-p
labormothers=c("marriedmothers2012labor")

#Lines for mothers no legend

#1.1 Lines for mothers 
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Mfraclabor12, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+labs(title="", x="Age", y="Proportion working")
p<-p+ylim(0,1)
#p<-p+xl12
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('MothermarriedagelineNOLEGEND.pdf')
pline
dev.off()

#1.2 Lines for fathers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Ffraclabor12, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+labs(title="Married fathers 2012",x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p+ylim(0,1)
pline<-p
dev.set()
pdf('Fathermarriedageline.pdf')
pline
dev.off()
marriedfathers2012labor<-p
laborfathers=c("marriedfathers2012labor")


#Fathers no legend
#1.2 Lines for fathers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Ffraclabor12, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+labs(title="",x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p+ylim(0,1)
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('FathermarriedagelineNOLINE.pdf')
pline
dev.off()

#2.1 Bars for mothers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Mfraclabor12, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+labs(title="Married mothers 2012",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('Mothermarriedagebar.pdf')
pbar
dev.off()
marriedmothers2012laborBAR<-p
labormothers=append(labormothers,"marriedmothers2012laborBAR")

#2.2 Bars for fathers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Ffraclabor12, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+labs(title="2012",x="Age", y="Proportion working")
p<-p+ylim(0,1)
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('Fathermarriedagebar.pdf.pdf')
pbar
dev.off()
marriedfathers2012laborBAR<-p
laborfathers=append(laborfathers,"marriedfathers2012laborBAR")



#----2010-----#
#1.1 Lines for mothers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Mfraclabor10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+ylim(0,1)
#p<-p+xl10
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Mothermarriedageline10.pdf')
pline
dev.off()
marriedmothers2010labor<-p
labormothers=append(labormothers,"marriedmothers2010labor")

#Lines for mothers no legend
#1.1 Lines for mothers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Mfraclabor10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+ylim(0,1)
#p<-p+xlim(c(16,45))
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('Mothermarriedageline10NOLEGEND.pdf')
pline
dev.off()

#1.2 Lines for fathers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Ffraclabor10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="2010" ,x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Fathermarriedageline10.pdf')
pline
dev.off()
marriedfathers2010labor<-p
laborfathers=append(laborfathers,"marriedfathers2010labor")


#1.2 Lines for fathers no legend
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Ffraclabor10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="" ,x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Fathermarriedageline10NOLEGEND.pdf')
pline
dev.off()



#2.1 Bars for mothers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Mfraclabor10, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('MothermarriedageCHILDbar10.pdf')
pbar
dev.off()
marriedmothers2010laborBAR<-p
labormothers=append(labormothers,"marriedmothers2010laborBAR")

#2.2 Bars for fathers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Ffraclabor10, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('FathermarriedageCHILDbar10.pdf.pdf')
pbar
dev.off()
marriedfathers2010laborBAR<-p
laborfathers=append(laborfathers,"marriedfathers2010laborBAR")



#2. By  age group. (5 years age group) 22-26;27-31;32-36;37-41;42-46;47-51

NUMBERGROUPS<-6
step<-5
length<-5
MINAGE=22
MAXAGE=49
RANK=MAXAGE-MINAGE
seq.age<-c("22-26","27-31","32-36","37-41","42-46","47-51",
           "22-26","27-31","32-36","37-41","42-46","47-51")

seq.Ffraclabor12<-vector(mode="numeric",length=NUMBERGROUPS*2)
seq.Mfraclabor12<-vector(mode="numeric",length=NUMBERGROUPS*2)
seq.Ffraclabor10<-vector(mode="numeric",length=NUMBERGROUPS*2)
seq.Mfraclabor10<-vector(mode="numeric",length=NUMBERGROUPS*2)
seq.indpred<-vector(mode="numeric",length=NUMBERGROUPS*2)

for (yy in 1:6){
  ttpred=yy+6
  seq.indpred[yy]="Observed"
  seq.indpred[ttpred]="Predicted"
  temp<-subset(PREDFINALMARR, data.Mage12>=MINAGE+(yy-1)*length & data.Mage12<MINAGE+(yy)*length)
  temp2<-subset(PREDFINALMARR, data.Fage12>=MINAGE+(yy-1)*length & data.Fage12<MINAGE+(yy)*length)
  #2012#
  #MOTHERS
  seq.Mfraclabor12[yy]<-mean(temp$data.Mfraclabor12)
  seq.Mfraclabor12[ttpred]<-mean(temp$Mfraclabor12PRED)
  #FATHERS
  seq.Ffraclabor12[yy]<-mean(temp2$data.Ffraclabor12)
  seq.Ffraclabor12[ttpred]<-mean(temp2$Ffraclabor12PRED)
  #2010
  #Mothers
  temp<-subset(PREDFINALMARR, data.Mage10>=MINAGE+(yy-1)*length & data.Mage10<MINAGE+(yy)*length)
  seq.Mfraclabor10[yy]<-mean(temp$data.Mfraclabor10)
  seq.Mfraclabor10[ttpred]<-mean(temp$Mfraclabor10PRED)
  #Fathers
  temp2<-subset(PREDFINALMARR, data.Fage10>=MINAGE+(yy-1)*length & data.Fage10<MINAGE+(yy)*length)
  seq.Ffraclabor10[yy]<-mean(temp2$data.Ffraclabor10)
  seq.Ffraclabor10[ttpred]<-mean(temp2$Ffraclabor10PRED)
}


seq.indpred<-as.factor(seq.indpred)
plot.Mfraclabor12mar<-data.frame(seq.Mfraclabor12,seq.indpred,seq.age)
plot.Mfraclabor10mar<-data.frame(seq.Mfraclabor10,seq.indpred,seq.age)
plot.Ffraclabor12mar<-data.frame(seq.Ffraclabor12,seq.indpred,seq.age)
plot.Ffraclabor10mar<-data.frame(seq.Ffraclabor10,seq.indpred,seq.age)

##====##
##2012##
##====##


#2.1 Lines
#Mothers
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.age, y=seq.Mfraclabor12, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="2012",x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
pdf('plineAGEGROUP2.pdf')
pline
dev.off()
marriedmothersed12<-p
labormothers=append(labormothers,"marriedmothersed12")

#Fathers
p<-ggplot(data=plot.Ffraclabor12mar, aes(x=seq.age, y=seq.Ffraclabor12, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="2012",x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
pdf('plineAGEGROUP2FATHER.pdf')
pline
dev.off()
marriedfathersed12<-p
laborfathers=append(laborfathers,"marriedfathersed12")



#2.2 Bars
#Mothers
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.age, y=seq.Mfraclabor12, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2012",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p 
dev.set()
pdf('pbarAGEGROUP2.pdf')
pbar
dev.off()
marriedmothersed12BAR<-p
labormothers=append(labormothers,"marriedmothersed12BAR")


#Fathers
p<-ggplot(data=plot.Ffraclabor12mar, aes(x=seq.age, y=seq.Ffraclabor12, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2012",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p 
dev.set()
pdf('pbarAGEGROUP2FATHER.pdf')
pbar
dev.off()
marriedfathersed12BAR<-p
laborfathers=append(laborfathers,"marriedfathersed12BAR")





##====##
##2010##
##====##



#2.1 Lines
#MOTHERS
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.age, y=seq.Mfraclabor10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
pdf('plineAGEGROUP1.pdf')
pline
dev.off()
marriedmothersed10<-p
labormothers=append(labormothers,"marriedmothersed10")

#FATHERS
p<-ggplot(data=plot.Ffraclabor10mar, aes(x=seq.age, y=seq.Ffraclabor10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
pdf('plineAGEGROUP1FATHER.pdf')
pline
dev.off()
marriedfathersed10<-p
laborfathers=append(laborfathers,"marriedfathersed10")

#2.2 Bars
#MOTHERS
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.age, y=seq.Mfraclabor10, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('pbarAGEGROUP1.pdf')
pbar
dev.off()
marriedmothersed10BAR<-p
labormothers=append(labormothers,"marriedmothersed10BAR")

#FATHERS
p<-ggplot(data=plot.Ffraclabor10mar, aes(x=seq.age, y=seq.Ffraclabor10, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Age", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('pbarAGEGROUP1FATHER.pdf')
pbar
dev.off()
marriedfathersed10BAR<-p
laborfathers=append(laborfathers,"marriedfathersed10BAR")

#3. By  Educational groups
NUMBERGROUPS<-3
step<-1
length<-1
RANK=MAXAGE-MINAGE
seq.indpred<-0*seq(1,NUMBERGROUPS*2,1)
seq.schools<-c(" Less than high school","High School","More than HS",
               " Less than high school","High School","More than HS")

seq.Mfraclabor12ED<-vector(mode="numeric",length=NUMBERGROUPS*2)
seq.Mfraclabor10ED<-vector(mode="numeric",length=NUMBERGROUPS*2)


#Less than high school
#2012
temp<-subset(PREDFINALMARR, data.Myrschool12<12)
seq.Mfraclabor12ED[1]<-mean(temp$data.Mfraclabor12)
seq.indpred[1]="Observed"
seq.Mfraclabor12ED[4]<-mean(temp$Mfraclabor12PRED)
seq.indpred[4]="Predicted"
#2010
temp<-subset(PREDFINALMARR, data.Myrschool12<12)
seq.Mfraclabor10ED[1]<-mean(temp$data.Mfraclabor10)
seq.indpred[1]="Observed"
seq.Mfraclabor10ED[4]<-mean(temp$Mfraclabor10PRED)
seq.indpred[4]="Predicted"

#High school
temp<-subset(PREDFINALMARR, data.Myrschool12==12)
#2012
seq.Mfraclabor12ED[2]<-mean(temp$data.Mfraclabor12)
seq.indpred[2]="Observed"
seq.Mfraclabor12ED[5]<-mean(temp$Mfraclabor12PRED)
seq.indpred[5]="Predicted"
#2010
seq.Mfraclabor10ED[2]<-mean(temp$data.Mfraclabor10)
seq.indpred[2]="Observed"
seq.Mfraclabor10ED[5]<-mean(temp$Mfraclabor10PRED)
seq.indpred[5]="Predicted"

#College
temp<-subset(PREDFINALMARR, data.Myrschool12>12)
#2012
seq.Mfraclabor12ED[3]<-mean(temp$data.Mfraclabor12)
seq.indpred[3]="Observed"
seq.Mfraclabor12ED[6]<-mean(temp$Mfraclabor12PRED)
seq.indpred[6]="Predicted"
plot.Mfraclabor12mar<-data.frame(seq.Mfraclabor12ED,seq.indpred,seq.schools)
#2010
seq.Mfraclabor10ED[3]<-mean(temp$data.Mfraclabor10)
seq.indpred[3]="Observed"
seq.Mfraclabor10ED[6]<-mean(temp$Mfraclabor10PRED)
seq.indpred[6]="Predicted"
seq.indpred<-as.factor(seq.indpred)
plot.Mfraclabor10mar<-data.frame(seq.Mfraclabor10ED,seq.indpred,seq.schools)

#3.1 Lines

#2012
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.schools, y=seq.Mfraclabor12ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Married mothers 2012",x="Schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
pdf('plineEDUGROUP2.pdf')
pline
dev.off()
marriedmothersedlineed2012<-p
labormothers=append(labormothers,"marriedmothersedlineed2012")

#2010
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.schools, y=seq.Mfraclabor10ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Married mothers 2010",x="Schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
pdf('plineEDUGROUP1.pdf')
pline
dev.off()
marriedmothersedlineed2010<-p
labormothers=append(labormothers,"marriedmothersedlineed2010")
#3.2 Bars
#2012
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.schools, y=seq.Mfraclabor12ED, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="",x="Shooling", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('pbarSCHOOLINGROUP2.pdf')
pbar
dev.off()
MarriedmothersED2012BAR<-p
labormothers=append(labormothers,"MarriedmothersED2012BAR")

#2010
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.schools, y=seq.Mfraclabor10ED, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="",x="Shooling", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p
dev.set()
pdf('pbarSCHOOLINGROUP1.pdf')
pbar
dev.off()
MarriedmothersED2010BAR<-p
labormothers=append(labormothers,"MarriedmothersED2010BAR")



#4. Educational by years of schooling
MINEDU=0
MAXEDU=20
RANK=MAXEDU-MINEDU
NUMBERGROUPS<-21
step<-1
length<-1
seq.indpred<-seq(MINEDU,2*RANK+1,1)
seq.schools<-seq(MINEDU,2*RANK+1,1)
seq.Mfraclabor12ED<-0*seq(MINEDU,2*RANK+1,1)
seq.Mfraclabor10ED<-0*seq(MINEDU,2*RANK+1,1)
seq.Ffraclabor12ED<-0*seq(MINEDU,2*RANK+1,1)
seq.Ffraclabor10ED<-0*seq(MINEDU,2*RANK+1,1)

for (yy in MINEDU:MAXEDU){
  tt<-yy-MINEDU+1
  ttpred=tt+MAXEDU-MINEDU+1
  temp<-subset(PREDFINALMARR,data.Myrschool12==yy)
  seq.Mfraclabor12ED[tt]<-mean(temp$data.Mfraclabor12)
  seq.Mfraclabor10ED[tt]<-mean(temp$data.Mfraclabor10)
  seq.Mfraclabor12ED[ttpred]<-mean(temp$Mfraclabor12PRED)
  seq.Mfraclabor10ED[ttpred]<-mean(temp$Mfraclabor10PRED)
  
  temp<-subset(PREDFINALMARR,data.Fyrschool12==yy)
  seq.Ffraclabor12ED[tt]<-mean(temp$data.Ffraclabor12)
  seq.Ffraclabor10ED[tt]<-mean(temp$data.Ffraclabor10)
  seq.Ffraclabor12ED[ttpred]<-mean(temp$Ffraclabor12PRED)
  seq.Ffraclabor10ED[ttpred]<-mean(temp$Ffraclabor10PRED)
  
  
  seq.indpred[ttpred]="Predicted"
  seq.indpred[tt]="Observed"
  seq.schools[ttpred]=yy
}
seq.indpred<-as.factor(seq.indpred)
plot.Mfraclabor12mar<-data.frame(seq.Mfraclabor12ED,seq.indpred,seq.schools)
plot.Mfraclabor10mar<-data.frame(seq.Mfraclabor10ED,seq.indpred,seq.schools)
plot.Ffraclabor12mar<-data.frame(seq.Ffraclabor12ED,seq.indpred,seq.schools)
plot.Ffraclabor10mar<-data.frame(seq.Ffraclabor10ED,seq.indpred,seq.schools)

#4.1 lines plot
#Mother
#2012
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.schools, y=seq.Mfraclabor12ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="2012",x="Years of schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)))
pline<-p
dev.set()
pdf('Mothereduline2.pdf')
pline
dev.off()
Marriedmothersed2012_2<-p
labormothers=append(labormothers,"Marriedmothersed2012_2")
#2010
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.schools, y=seq.Mfraclabor10ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="2010",x="Years of schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)))
pline<-p
dev.set()
pdf('Mothereduline1.pdf')
pline
dev.off()
Marriedmothersed2010_2<-p
labormothers=append(labormothers,"Marriedmothersed2010_2")
#4.1 lines plot no legend
#Mother
#2012
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.schools, y=seq.Mfraclabor12ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="",x="Years of schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(3)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('Mothereduline2NOLEGEND.pdf')
pline
dev.off()
Marriedmothersed2012_2<-p
labormothers=append(labormothers,"Marriedmothersed2012_2")




#Bar
#2.2 Bars
p<-ggplot(data=plot.Mfraclabor12mar, aes(x=seq.schools, y=seq.Mfraclabor12ED, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2012",x="Years of Schooling", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p 
dev.set()
pdf('Motheredubar2.pdf')
pbar
dev.off()


#Father
#2012
#Line
p<-ggplot(data=plot.Ffraclabor12mar, aes(x=seq.schools, y=seq.Ffraclabor12ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="2012",x="Years of schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)))
pline<-p
dev.set()
pdf('Fathereduline2.pdf')
pline
dev.off()
Marriedfathersed2012_2<-p
laborfathers=append(laborfathers,"Marriedfathersed2012_2")

#2010
p<-ggplot(data=plot.Ffraclabor10mar, aes(x=seq.schools, y=seq.Ffraclabor10ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="2010",x="Years of schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)))
pline<-p
dev.set()
pdf('Fathereduline1.pdf')
pline
dev.off()
Marriedfathersed2012_2<-p
laborfathers=append(laborfathers,"Marriedfathersed2012_2")


#Father 
#Line NO LEGEND
p<-ggplot(data=plot.Ffraclabor12mar, aes(x=seq.schools, y=seq.Ffraclabor12ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="",x="Years of schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(3)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('Fathereduline2NOLEGEND.pdf')
pline
dev.off()
Marriedfathersed2012_2<-p
laborfathers=append(laborfathers,"Marriedfathersed2012_2")

#Bar
p<-ggplot(data=plot.Ffraclabor12mar, aes(x=seq.schools, y=seq.Ffraclabor12ED, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="2012",x="Years of Schooling", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p 
dev.set()
pdf('Fatheredubar2.pdf')
pbar
dev.off()


#2010 mothers no legend
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.schools, y=seq.Mfraclabor10ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="",x="Years of Schooling", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('Mothereduline1NOLEGEND.pdf')
pline
dev.off()



#Bars
#2.2 Bars
p<-ggplot(data=plot.Mfraclabor10mar, aes(x=seq.schools, y=seq.Mfraclabor10ED, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Years of Schooling", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p 
dev.set()
pdf('Motheredubar1.pdf')
pbar
dev.off()

#2010 fathers
#line
p<-ggplot(data=plot.Ffraclabor10mar, aes(x=seq.schools, y=seq.Ffraclabor10ED, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+xlim(2,20)
p<-p+labs(title="",x="Education", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.position="none")
pline<-p
dev.set()
pdf('Fathereduline1NOLEGEND.pdf')
pline
dev.off()
Marriedfathersed2010_2<-p
laborfathers=append(laborfathers,"Marriedfathersed2010_2")

#bar
#Bar
p<-ggplot(data=plot.Ffraclabor10mar, aes(x=seq.schools, y=seq.Ffraclabor12ED, group=seq.indpred, fill=seq.indpred))
p<-p+geom_bar(stat="identity", position="dodge")
p<-p+ylim(0,1)
p<-p+labs(title="2010",x="Years of Schooling", y="Proportion working")
p<-p+scale_fill_manual(name = "", values = c("red","blue"))
pbar<-p 
dev.set()
pdf('Fatheredubar1.pdf')
pbar
dev.off()

#=============================
#Childcare decisions
#=============================

#Childcare decisions by single age group
MINAGE=30
MAXAGE=58
RANK=MAXAGE-MINAGE
seq.agechild<-c(seq(MINAGE,2*RANK,1))
seq.Cchildcare10<-0*seq(1,2*RANK,1)
seq.indpred<-0*seq(MINAGE,2*RANK,1)

for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Cedad_meses10==yy)
  #2010
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}

seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)

#Plotting the results
#--------------------#
#All married mothers #
#--------------------#

#1.1 Lines for mothers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="", x="Age of child", y="Proportion working")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1.pdf')
pline
dev.off()
childcareagechildmarried<-p
childcarepred<-c("childcareagechildmarried")




#===========================
#Now doing it by working category of mother
#===========================
#Working mothers
for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Cedad_meses10==yy & data.Mfraclabor10==1)
  #2010
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}

seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)

#Plotting the results
#==============================
#1.1 Lines for Working mothers
#==============================
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Working mothers",x="Age of child", y="Proportion in childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1working.pdf')
pline
dev.off()
childcareagechildmarriedworking<-p
childcarepred<-append(childcarepred,"childcareagechildmarriedworking")


#===========================
#Now working mothers
#===========================
for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Cedad_meses10==yy & data.Mfraclabor10==0)
  #2010
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}

seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)

#Plotting the results
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Non-working mothers",x="Age of child", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1Notworking.pdf')
pline
dev.off()
childcareagechildmarriednotworking<-p
childcarepred<-append(childcarepred,"childcareagechildmarriednotworking")


#=======================#
#Childcare by age of mom#
#=======================#

#Labor market decisions By individual age group
MINAGE=22
MAXAGE=49
RANK=MAXAGE-MINAGE
seq.age<-c(seq(MINAGE,2*RANK,1))
seq.Mfraclabor12<-0*seq(MINAGE,2*RANK,1)
seq.Mfraclabor10<-0*seq(MINAGE,2*RANK,1)
seq.Cchildcare10<-0*seq(MINAGE,2*RANK,1)
seq.Ffraclabor12<-0*seq(MINAGE,2*RANK,1)
seq.Ffraclabor10<-0*seq(MINAGE,2*RANK,1)
seq.indpred<-0*seq(MINAGE,2*RANK,1)

for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Mage12==yy)
  #Childcareinfo
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}
seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)


#Plotting the results
#--------------------#
#All married mothers #
#--------------------#

#1.1 Lines for mothers
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="", x="Age", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1.pdf')
pline
dev.off()
childcareagemommarried<-p
childcarepred<-append(childcarepred,"childcareagemommarried")




#===========================
#Now doing it by working category of mother
#===========================
MINAGE=22
MAXAGE=49
RANK=MAXAGE-MINAGE
seq.age<-c(seq(MINAGE,2*RANK,1))
seq.Mfraclabor12<-0*seq(MINAGE,2*RANK,1)
seq.Mfraclabor10<-0*seq(MINAGE,2*RANK,1)
seq.Cchildcare10<-0*seq(MINAGE,2*RANK,1)
seq.Ffraclabor12<-0*seq(MINAGE,2*RANK,1)
seq.Ffraclabor10<-0*seq(MINAGE,2*RANK,1)
seq.indpred<-0*seq(MINAGE,2*RANK,1)
#Working mothers


for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Mage12==yy & data.Mfraclabor10==1)
  #Childcareinfo
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}

seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)

#Plotting the results
#==============================
#1.1 Lines for Working mothers
#==============================
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Working mothers",x="Age", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1working.pdf')
pline
dev.off()
childcareagemommarriedworking<-p
childcarepred<-append(childcarepred,"childcareagemommarriedworking")


#===========================
#Now working mothers
#===========================

for (yy in MINAGE:MAXAGE){
  tt<-yy-MINAGE+1
  ttpred=tt+(MAXAGE-MINAGE+1)
  #Extracting mother's mean
  temp<-subset(PREDFINALMARR, data.Mage12==yy & data.Mfraclabor10==0)
  #Childcareinfo
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}
seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)

#Plotting the results
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Non-working mothers",x="Age", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1Notworking.pdf')
pline
dev.off()
childcareagemommarriednotworking<-p
childcarepred<-append(childcarepred,"childcareagemommarriednotworking")


#===========================================#
#Childcare decissions by education of mother#
#===========================================#

#4. Educational by years
MINEDU=0
MAXEDU=20
RANK=MAXEDU-MINEDU
NUMBERGROUPS<-21
step<-1
length<-1
seq.indpred<-seq(MINEDU,2*RANK+1,1)
seq.schools<-seq(MINEDU,2*RANK+1,1)
seq.Mfraclabor12ED<-0*seq(MINEDU,2*RANK+1,1)
seq.age<-0*seq(MINEDU,2*RANK+1,1)
seq.Mfraclabor10ED<-0*seq(MINEDU,2*RANK+1,1)
seq.Ffraclabor12ED<-0*seq(MINEDU,2*RANK+1,1)
seq.Ffraclabor10ED<-0*seq(MINEDU,2*RANK+1,1)
seq.Cchildcare10<-0*seq(MINEDU,2*RANK+1,1)
seq.Cchildcare10ED<-0*seq(MINEDU,2*RANK+1,1)

for (yy in MINEDU:MAXEDU){
  tt<-yy-MINEDU+1
  ttpred=tt+MAXEDU-MINEDU+1
  #Getting the subset of married and schooling according
  temp<-subset(PREDFINALMARR,data.Myrschool12==yy)
  
  #Childcareinfo
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}
seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)
#Plotting the results
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="",x="Age", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1Notworking.pdf')
pline
dev.off()
childcareedmomall<-p
childcarepred<-append(childcarepred,"childcareedmomall")

#================#
#Working mothers #
#================#
for (yy in MINEDU:MAXEDU){
  tt<-yy-MINEDU+1
  ttpred=tt+MAXEDU-MINEDU+1
  #Getting the subset of married and schooling according
  temp<-subset(PREDFINALMARR,data.Myrschool12==yy  & data.Mfraclabor10==1)
  
  #Childcareinfo
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}
seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)
#Plotting the results
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Working women",x="Age", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1Notworking.pdf')
pline
dev.off()
childcareworkinged<-p
childcarepred<-append(childcarepred,"childcareworkinged")


#====================#
#Not Working mothers #
#====================#
for (yy in MINEDU:MAXEDU){
  tt<-yy-MINEDU+1
  ttpred=tt+MAXEDU-MINEDU+1
  #Getting the subset of married and schooling according
  temp<-subset(PREDFINALMARR,data.Myrschool12==yy  & data.Mfraclabor10==0)
  
  #Childcareinfo
  seq.Cchildcare10[tt]<-mean(temp$data.Cchildcare10)
  seq.Cchildcare10[ttpred]<-mean(temp$predchildcare10)
  #Ages and final details
  seq.age[tt]=yy
  seq.indpred[tt]="Observed"
  seq.age[ttpred]=yy
  seq.indpred[ttpred]="Predicted"
}
seq.indpred<-as.factor(seq.indpred)
plot.fraclabormar<-data.frame(seq.Cchildcare10,seq.indpred,seq.age)
#Plotting the results
p<-ggplot(data=plot.fraclabormar, aes(x=seq.age, y=seq.Cchildcare10, group=seq.indpred, colour=seq.indpred))
p<-p+geom_line(size=sizegraphs)
p<-p+ylim(0,1)
p<-p+labs(title="Working women",x="Age", y="Proportion in Childcare")
p<-p+scale_colour_manual(name = "", values = c("red","blue"))
pline<-p
dev.set()
pdf('Childcare1Notworking.pdf')
pline
dev.off()
childcarenotworkinged<-p
childcarepred<-append(childcarepred,"childcarenotworkinged")


#3.1 Childcare by working and not-working mothers
time<-c("Working","Working","Not-working","Not-working")
seqpred<-c("Predicted","Observed","Predicted","Observed")
childcare<-c(0,0,0,0)
#Filling in childcare predicted and observed
temp<-subset(PREDFINALMARR, data.Mfraclabor10==0)
Resultado4=paste("Not working mothers: Childcare decision total predicted->",mean(temp$predchildcare10),"observed->",mean(temp$data.Cchildcare10))
childcare[3]=mean(temp$predchildcare10)
childcare[4]=mean(temp$data.Cchildcare10)
temp<-subset(PREDFINALMARR, data.Mfraclabor10==1)
childcare[1]=mean(temp$predchildcare10)
childcare[2]=mean(temp$data.Cchildcare10)
seqpred<-as.factor(seqpred)
time<-as.factor(time)
df<-data.frame(time,seqpred,childcare)

p<-ggplot(data=df,aes(x=time,y=childcare,group=seqpred,fill=seqpred))
p<-p+geom_bar(stat="identity",position="dodge")
p<-p+labs(title="Childcare attendance", x="Work status",y="Proportion attending")
p<-p+scale_fill_manual(name="",values=c("red","blue"))
p<-p+ylim(0,1)
pbar<-p
dev.set()
pdf('Childcareresults.pdf')
pbar
dev.off()
childcarBARS<-p
childcarepred<-append(childcarepred,"childcarBARS")






#====================================#
#Putting all the predictions together#
#====================================#
setwd(finaldir)
#labormothers, laborfathers, childcarepred
#1. Multiplot of mother's labor force participation - married
dev.set()
png(file="MotherMarriedLabor.png",width=1600,height=850)
multiplot(marriedmothers2012labor,marriedmothers2012laborBAR,marriedmothers2010labor,marriedmothers2010laborBAR,
          marriedmothersed12,marriedmothersed12BAR,marriedmothersed10,marriedmothersed10BAR,marriedmothersedlineed2012,
          marriedmothersedlineed2010,MarriedmothersED2012BAR,MarriedmothersED2010BAR,Marriedmothersed2012_2,Marriedmothersed2010_2,
          cols=5,title="A")
dev.off()


#2. Multiplot of father's labor force participation - married
dev.set()
png(file="FatherMarriedLabor.png",width=1600,height=850)
multiplot(marriedfathers2012labor,marriedfathers2012laborBAR,marriedfathers2010labor,marriedfathers2010laborBAR,
          Marriedfathersed2012_2,Marriedfathersed2010_2,marriedfathersed12,marriedfathersed12BAR,
          marriedfathersed10,marriedfathersed10BAR,
          cols=3,title="A")
dev.off()

#3. Childcare
dev.set()
png(file="ChildcareMarried.png",width=1600,height=850)
multiplot(childcareagechildmarried,childcareagechildmarriedworking,childcareagechildmarriednotworking,childcareagemommarried,
          childcareagemommarriedworking,childcareagemommarriednotworking,childcareedmomall,childcareworkinged,childcarenotworkinged,childcarBARS,
          cols=3,title="A")
dev.off()
setwd(initialdir)


#Male labor force participation
Resultado1=paste("Male labor force participation predicted 2012->",mean(Ffraclabor12PRED),"observed",mean(data.Ffraclabor12))
Resultado2=paste("Male labor force participation predicted 2010->",mean(Ffraclabor10PRED),"observed",mean(data.Ffraclabor10))

#Childcare decisions
temp<-subset(PREDFINALMARR)
Resultado3=paste("Childcare decision total predicted->",mean(temp$predchildcare10),"observed->",mean(temp$data.Cchildcare10))

#By labor supply
temp<-subset(PREDFINALMARR, data.Mfraclabor10==0)
Resultado4=paste("Not working mothers: Childcare decision total predicted->",mean(temp$predchildcare10),"observed->",mean(temp$data.Cchildcare10))


temp<-subset(PREDFINALMARR, data.Mfraclabor10==1)
Resultado5=paste("Working mothers: Childcare decision total predicted->",mean(temp$predchildcare10),"observed->",mean(temp$data.Cchildcare10))


Resultado6=paste("Female labor force participation predicted 2012->",mean(Mfraclabor12PRED),"observed",mean(data.Mfraclabor12))
Resultado7=paste("Female labor force participation predicted 2010->",mean(Mfraclabor10PRED),"observed",mean(data.Mfraclabor10))

Resultado8=paste("sd of skills",sqrt(var(log(Cskills12PRED))))


ResultadoTotal=rbind(Resultado1,Resultado2,Resultado3,Resultado4,Resultado5,Resultado6,Resultado7,Resultado8)
write(ResultadoTotal,file="SummaryModelfit.txt")


#I Putting the results in a table#
DT=2 #Digits of precision in table
#I.1 Labor force participation in 2010
Table1<-matrix(0,2,2)
rownames(Table1)<-c("Mothers","Fathers")
colnames(Table1)<-c("Predicted","Observed")
Table1[1,1]<-100*mean(Mfraclabor10PRED)
Table1[1,2]<-100*mean(data.Mfraclabor10)
Table1[2,1]<-100*mean(Ffraclabor10PRED)
Table1[2,2]<-100*mean(data.Ffraclabor10)
#Column of correct predictions not included as not good
sum(data.Mfraclabor10==Mfraclabor10PRED)/length(Mfraclabor10PRED)
sum(data.Mfraclabor12==Mfraclabor12PRED)/length(Mfraclabor12PRED)
sum(data.Ffraclabor10==Ffraclabor10PRED)/length(Ffraclabor10PRED)
sum(data.Ffraclabor12==Ffraclabor12PRED)/length(Ffraclabor12PRED)
Table1LATEX<-xtable(Table1,caption="Labor supply 2010",digits=DT,table.placement="H",label="tab:ModelfitLabor2010")
align(Table1LATEX)<-"ccc"
write(print(Table1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="ModelfitLabor2010.tex")
#Nocaption
Table1LATEX<-xtable(Table1,digits=DT,table.placement="H",label="tab:ModelfitLabor2010")
write(print(Table1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitLabor2010NOCAPTION.tex")

#I.2 Labor force participation in 2012
Table2<-matrix(0,2,2)
rownames(Table2)<-c("Mothers","Fathers")
colnames(Table2)<-c("Predicted","Observed")
Table2[1,1]<-100*mean(Mfraclabor12PRED)
Table2[1,2]<-100*mean(data.Mfraclabor12)
Table2[2,1]<-100*mean(Ffraclabor12PRED)
Table2[2,2]<-100*mean(data.Ffraclabor12)
Table2LATEX<-xtable(Table2,caption="Labor supply 2012",digits=DT,table.placement="H",label="tab:ModelfitLabor2012")
align(Table2LATEX)<-"ccc"
write(print(Table2LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="ModelfitLabor2012.tex")
#Nocaption
Table2LATEX<-xtable(Table2,digits=DT,table.placement="H",label="tab:ModelfitLabor2012")
write(print(Table2LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitLabor2012NOCAPTION.tex")


#I.3 Childcare services
Table3<-matrix(0,2,2)
rownames(Table3)<-c("Working mothers","Not working mothers")
colnames(Table3)<-c("Predicted","Observed")
Table3[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1)$data.Cchildcare10)
Table3[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1)$predchildcare10)
Table3[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0)$data.Cchildcare10)
Table3[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0)$predchildcare10)
Table3LATEX<-xtable(Table2,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(Table3LATEX)<-"lll"
write(print(Table3LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="Modelfitchildcare.tex")
#Nocaption
Table3LATEX<-xtable(Table3,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(Table3LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitchildcareNOCAPTION.tex")



#---Age Groups tables-----#

#----------------------#
#1. Ten years age group
#----------------------#


#Labor supply 2012
Table12FEM1<-matrix(0,2,2)
colnames(Table12FEM1)<-c("Predicted","Observed")
rownames(Table12FEM1)<-c("Under 30","Over 30")
Table12FEM1[1,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=20 & data.Mage12<=30)$Mfraclabor12PRED)
Table12FEM1[1,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=20 & data.Mage12<=30)$data.Mfraclabor12)
Table12FEM1[2,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=31 & data.Mage12<=40)$Mfraclabor12PRED)
Table12FEM1[2,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=31 & data.Mage12<=40)$data.Mfraclabor12)
Table12FEM1LATEX<-xtable(Table12FEM1,digits=DT,table.placement="H")
write(print(Table12FEM1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="Table12FEM1LATEX.tex")

#Labor supply 2010
Table10FEM1<-matrix(0,2,2)
colnames(Table10FEM1)<-c("Predicted","Observed")
rownames(Table10FEM1)<-c("Under 30","Over 30")
Table10FEM1[1,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=20 & data.Mage10<=30)$Mfraclabor10PRED)
Table10FEM1[1,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=20 & data.Mage10<=30)$data.Mfraclabor10)
Table10FEM1[2,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=31 & data.Mage10<=41)$Mfraclabor10PRED)
Table10FEM1[2,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=31 & data.Mage10<=41)$data.Mfraclabor10)
Table10FEM1LATEX<-xtable(Table10FEM1,digits=DT,table.placement="H")
write(print(Table10FEM1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="Table10FEM1LATEX.tex")


#Childcare services 1 age group 20-30
TableChildcare1<-matrix(0,2,2)
rownames(TableChildcare1)<-c("Working mothers","Not working mothers")
colnames(TableChildcare1)<-c("Predicted","Observed")
TableChildcare1[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=20 & data.Mage10<=30)$data.Cchildcare10)
TableChildcare1[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=20 & data.Mage10<=30)$predchildcare10)
TableChildcare1[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=20 & data.Mage10<=30)$data.Cchildcare10)
TableChildcare1[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=20 & data.Mage10<=30)$predchildcare10)
TableCH1<-xtable(TableChildcare1,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(TableCH1)<-"lll"
write(print(TableCH1,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="TableCH1.tex")
#Nocaption
TableCH1<-xtable(TableCH1,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(TableCH1,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="TableCH1.tex")


#Childcare services 1 age group 30-40
TableChildcare2<-matrix(0,2,2)
rownames(TableChildcare2)<-c("Working mothers","Not working mothers")
colnames(TableChildcare2)<-c("Predicted","Observed")
TableChildcare2[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=30 & data.Mage10<=39)$data.Cchildcare10)
TableChildcare2[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=30 & data.Mage10<=39)$predchildcare10)
TableChildcare2[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=31 & data.Mage10<=39)$data.Cchildcare10)
TableChildcare2[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=31 & data.Mage10<=39)$predchildcare10)
TableCH2<-xtable(TableChildcare2,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(TableCH2)<-"lll"
write(print(TableCH2,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="TableCH2.tex")
#Nocaption
TableCH2<-xtable(TableCH2,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(TableCH2,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="TableCH2.tex")


#----------------------#
#1. Five years age group
#----------------------#


#Labor supply 2012
Table12FEMage5<-matrix(0,5,2)
colnames(Table12FEMage5)<-c("Predicted","Observed")
rownames(Table12FEMage5)<-c("20-30","31-40")
Table12FEMage5[1,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=20 & data.Mage12<=27)$Mfraclabor12PRED)
Table12FEMage5[2,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=28 & data.Mage12<=33)$Mfraclabor12PRED)
Table12FEMage5[3,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=34 & data.Mage12<=39)$Mfraclabor12PRED)
Table12FEMage5[4,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=40 & data.Mage12<=45)$Mfraclabor12PRED)
Table12FEMage5[5,1]=100*mean(subset(PREDFINALMARR,data.Mage12>=46 & data.Mage12<=55)$Mfraclabor12PRED)

Table12FEMage5[1,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=20 & data.Mage12<=27)$data.Mfraclabor12)
Table12FEMage5[2,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=28 & data.Mage12<=33)$data.Mfraclabor12)
Table12FEMage5[3,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=34 & data.Mage12<=39)$data.Mfraclabor12)
Table12FEMage5[4,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=40 & data.Mage12<=45)$data.Mfraclabor12)
Table12FEMage5[5,2]=100*mean(subset(PREDFINALMARR,data.Mage12>=46 & data.Mage12<=55)$data.Mfraclabor12)

Table12FEMage5LATEX<-xtable(Table12FEMage5,digits=DT,table.placement="H")
write(print(Table12FEMage5LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="Table12FEMage5.tex")

#Labor supply 2010
Table10FEMage5<-matrix(0,5,2)
colnames(Table10FEMage5)<-c("Predicted","Observed")
rownames(Table10FEMage5)<-c("20-30","31-40")
Table10FEMage5[1,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=20 & data.Mage10<=27)$Mfraclabor10PRED)
Table10FEMage5[2,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=28 & data.Mage10<=33)$Mfraclabor10PRED)
Table10FEMage5[3,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=34 & data.Mage10<=39)$Mfraclabor10PRED)
Table10FEMage5[4,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=40 & data.Mage10<=45)$Mfraclabor10PRED)
Table10FEMage5[5,1]=100*mean(subset(PREDFINALMARR,data.Mage10>=46 & data.Mage10<=55)$Mfraclabor10PRED)

Table10FEMage5[1,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=20 & data.Mage10<=27)$data.Mfraclabor10)
Table10FEMage5[2,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=28 & data.Mage10<=33)$data.Mfraclabor10)
Table10FEMage5[3,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=34 & data.Mage10<=39)$data.Mfraclabor10)
Table10FEMage5[4,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=40 & data.Mage10<=45)$data.Mfraclabor10)
Table10FEMage5[5,2]=100*mean(subset(PREDFINALMARR,data.Mage10>=46 & data.Mage10<=55)$data.Mfraclabor10)

Table10FEMage5LATEX<-xtable(Table10FEMage5,digits=DT,table.placement="H")
write(print(Table12FEMage5LATEX,Table10FEMage5LATEX=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="Table10FEMage5.tex")


#Childcare services 1 age group 20-30
TableChildcare1<-matrix(0,2,2)
rownames(TableChildcare1)<-c("Working mothers","Not working mothers")
colnames(TableChildcare1)<-c("Predicted","Observed")
TableChildcare1[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=20 & data.Mage10<=30)$data.Cchildcare10)
TableChildcare1[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=20 & data.Mage10<=30)$predchildcare10)
TableChildcare1[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=20 & data.Mage10<=30)$data.Cchildcare10)
TableChildcare1[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=20 & data.Mage10<=30)$predchildcare10)
TableCH1<-xtable(TableChildcare1,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(TableCH1)<-"lll"
write(print(TableCH1,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="TableCH1.tex")
#Nocaption
TableCH1<-xtable(TableCH1,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(TableCH1,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="TableCH1.tex")


#Childcare services 1 age group 30-40
TableChildcare2<-matrix(0,2,2)
rownames(TableChildcare2)<-c("Working mothers","Not working mothers")
colnames(TableChildcare2)<-c("Predicted","Observed")
TableChildcare2[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=30 & data.Mage10<=39)$data.Cchildcare10)
TableChildcare2[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=30 & data.Mage10<=39)$predchildcare10)
TableChildcare2[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=31 & data.Mage10<=39)$data.Cchildcare10)
TableChildcare2[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=31 & data.Mage10<=39)$predchildcare10)
TableCH2<-xtable(TableChildcare2,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(TableCH2)<-"lll"
write(print(TableCH2,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="TableCH2.tex")
#Nocaption
TableCH2<-xtable(TableCH2,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(TableCH2,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="TableCH2.tex")



#===================#
#Tables by education#
#===================#

#Labor supply 2012
Table12FEM1EDU1<-matrix(0,2,2)
colnames(Table12FEM1EDU1)<-c("Predicted","Observed")
rownames(Table12FEM1EDU1)<-c("Less than college","College educated")
Table12FEM1EDU1[1,1]=100*mean(subset(PREDFINALMARR,data.Myrschool12>12)$Mfraclabor12PRED)
Table12FEM1EDU1[1,2]=100*mean(subset(PREDFINALMARR,data.Myrschool12>12)$data.Mfraclabor12)
Table12FEM1EDU1[2,1]=100*mean(subset(PREDFINALMARR,data.Myrschool12<=12)$Mfraclabor12PRED)
Table12FEM1EDU1[2,2]=100*mean(subset(PREDFINALMARR,data.Myrschool12<=12)$data.Mfraclabor12)
Table12FEM1EDU1LATEX<-xtable(Table12FEM1,digits=DT,table.placement="H")
write(print(Table12FEM1EDU1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="Table12FEM1EDU1LATEX.tex")


#Labor supply 2012
Table10FEM1EDU1<-matrix(0,2,2)
colnames(Table10FEM1EDU1)<-c("Predicted","Observed")
rownames(Table10FEM1EDU1)<-c("Less than college","College educated")
Table10FEM1EDU1[1,1]=100*mean(subset(PREDFINALMARR,data.Myrschool12>12)$Mfraclabor10PRED)
Table10FEM1EDU1[1,2]=100*mean(subset(PREDFINALMARR,data.Myrschool12>12)$data.Mfraclabor10)
Table10FEM1EDU1[2,1]=100*mean(subset(PREDFINALMARR,data.Myrschool12<=12)$Mfraclabor10PRED)
Table10FEM1EDU1[2,2]=100*mean(subset(PREDFINALMARR,data.Myrschool12<=12)$data.Mfraclabor10)
Table10FEM1EDU1LATEX<-xtable(Table10FEM1EDU1,digits=DT,table.placement="H")
write(print(Table10FEM1EDU1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="Table10FEM1EDU1LATEX.tex")


#Childcare services 1 age group 20-30
TableChildcare1<-matrix(0,2,2)
rownames(TableChildcare1)<-c("Working mothers","Not working mothers")
colnames(TableChildcare1)<-c("Predicted","Observed")
TableChildcare1[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=20 & data.Mage10<=30)$data.Cchildcare10)
TableChildcare1[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=20 & data.Mage10<=30)$predchildcare10)
TableChildcare1[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=20 & data.Mage10<=30)$data.Cchildcare10)
TableChildcare1[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=20 & data.Mage10<=30)$predchildcare10)
TableCH1<-xtable(TableChildcare1,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(TableCH1)<-"lll"
write(print(TableCH1,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="TableCH1.tex")
#Nocaption
TableCH1<-xtable(TableCH1,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(TableCH1,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="TableCH1.tex")


#Childcare services 1 age group 30-40
TableChildcare2<-matrix(0,2,2)
rownames(TableChildcare2)<-c("Working mothers","Not working mothers")
colnames(TableChildcare2)<-c("Predicted","Observed")
TableChildcare2[1,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=30 & data.Mage10<=39)$data.Cchildcare10)
TableChildcare2[1,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==1 & data.Mage10>=30 & data.Mage10<=39)$predchildcare10)
TableChildcare2[2,1]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=31 & data.Mage10<=39)$data.Cchildcare10)
TableChildcare2[2,2]<-100*mean(subset(PREDFINALMARR,data.Mfraclabor10==0 & data.Mage10>=31 & data.Mage10<=39)$predchildcare10)
TableCH2<-xtable(TableChildcare2,caption="Childcare ",digits=DT,table.placement="H",label="tab:Modelfitchildcare")
align(TableCH2)<-"lll"
write(print(TableCH2,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="TableCH2.tex")
#Nocaption
TableCH2<-xtable(TableCH2,digits=DT,table.placement="H",label="tab:Modelfitchildcare")
write(print(TableCH2,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="TableCH2.tex")




#=======================================================================
#===============Distribution of wages#==================================
#=======================================================================

PREDWAGEMOTHER12<-rep(0,SIZE)
PREDWAGEMOTHER10<-rep(0,SIZE)
PREDWAGEFATHER12<-rep(0,SIZE)
PREDWAGEFATHER10<-rep(0,SIZE)

for(ii in 1:SIZE){
  PREDWAGEFATHER10[ii]=F_predwage(bbeta0f,bbeta1f,bbeta2f,bbeta3f,data.Fyrschool12[ii],data.Fage10[ii])
  PREDWAGEFATHER12[ii]=F_predwage(bbeta0f,bbeta1f,bbeta2f,bbeta3f,data.Fyrschool12[ii],data.Fage12[ii])
  PREDWAGEMOTHER10[ii]=F_predwage(bbeta0m,bbeta1m,bbeta2m,bbeta3m,data.Myrschool12[ii],data.Mage10[ii])
  PREDWAGEMOTHER12[ii]=F_predwage(bbeta0m,bbeta1m,bbeta2m,bbeta3m,data.Myrschool12[ii],data.Mage12[ii])
}


#Bandwidth selection
BW=1.5


#=======================
#Wages of mothers 2012
#=======================

WAGESMOTHER12OBSERVED=data.frame(log(data.Mwage12),rep("Observed",SIZE),data.Mfraclabor12)
WAGESMOTHER12OBSERVED=subset(WAGESMOTHER12OBSERVED,data.Mfraclabor12==1)
colnames(WAGESMOTHER12OBSERVED)<-c("logWages","Data","Labor supply")

WAGESMOTHER12PREDICTED=data.frame(log(PREDWAGEMOTHER12),rep("Predicted",SIZE),data.Mfraclabor12)
WAGESMOTHER12PREDICTED=subset(WAGESMOTHER12PREDICTED,data.Mfraclabor12==1)
colnames(WAGESMOTHER12PREDICTED)<-c("logWages","Data","Labor supply")


WAGESMOTHER12=rbind(WAGESMOTHER12PREDICTED,WAGESMOTHER12OBSERVED)
p<-ggplot(WAGESMOTHER12, aes(logWages,fill=Data,colour=Data)) +
  geom_density(adjust=BW,alpha=0.3)+labs(title="2012")
p<-p+xlim(c(5,20))
p
dev.set()
png(file="Mwages2012.png")
p
dev.off()


#======================
#Wages of mothers 2010
#======================

WAGESMOTHER10OBSERVED=data.frame(log(data.Mwage10),rep("Observed",SIZE),data.Mfraclabor10)
WAGESMOTHER10OBSERVED=subset(WAGESMOTHER10OBSERVED,data.Mfraclabor10==1)
colnames(WAGESMOTHER10OBSERVED)<-c("logWages","Data","Labor supply")

WAGESMOTHER10PREDICTED=data.frame(log(PREDWAGEMOTHER10),rep("Predicted",SIZE),data.Mfraclabor10)
WAGESMOTHER10PREDICTED=subset(WAGESMOTHER10PREDICTED,data.Mfraclabor10==1)
colnames(WAGESMOTHER10PREDICTED)<-c("logWages","Data","Labor supply")


WAGESMOTHER10=rbind(WAGESMOTHER10PREDICTED,WAGESMOTHER10OBSERVED)
BW=3
p<-ggplot(WAGESMOTHER10, aes(logWages,fill=Data,colour=Data)) +
  geom_density(adjust=BW,alpha=0.3)+labs(title="2010")
p<-p+xlim(c(5,20))
p<-p+labs(title="Women",x="Log-wages", y="Density")

p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.title=element_text(size=rel(0)))
p


dev.set()
png(file="Mwages2010.png")
p
dev.off()

#==============Wages of fathers#===================#

#=======================
#Wages of fathers 2012
#=======================

WAGESFATHER12OBSERVED=data.frame(log(data.Fwage12),rep("Observed",SIZE),data.Ffraclabor12)
WAGESFATHER12OBSERVED=subset(WAGESFATHER12OBSERVED,data.Ffraclabor12==1)
colnames(WAGESFATHER12OBSERVED)<-c("logWages","Data","Labor supply")

WAGESFATHER12PREDICTED=data.frame(log(PREDWAGEFATHER12),rep("Predicted",SIZE),data.Ffraclabor12)
WAGESFATHER12PREDICTED=subset(WAGESFATHER12PREDICTED,data.Mfraclabor12==1)
colnames(WAGESFATHER12PREDICTED)<-c("logWages","Data","Labor supply")


WAGESFATHER12=rbind(WAGESFATHER12PREDICTED,WAGESFATHER12OBSERVED)
p<-ggplot(WAGESFATHER12, aes(logWages,fill=Data,colour=Data)) +
  geom_density(adjust=BW,alpha=0.3)+labs(title="2012")
p<-p+xlim(c(5,20))

p

dev.set()
png(file="Fwages2012.png")
p
dev.off()


#======================
#Wages of FAthers 2010
#======================

WAGESFATHER10OBSERVED=data.frame(log(data.Fwage10),rep("Observed",SIZE),data.Ffraclabor10)
WAGESFATHER10OBSERVED=subset(WAGESFATHER10OBSERVED,data.Ffraclabor10==1)
colnames(WAGESFATHER10OBSERVED)<-c("logWages","Data","Labor supply")

WAGESFATHER10PREDICTED=data.frame(log(PREDWAGEFATHER10),rep("Predicted",SIZE),data.Ffraclabor10)
WAGESFATHER10PREDICTED=subset(WAGESFATHER10PREDICTED,data.Ffraclabor10==1)
colnames(WAGESFATHER10PREDICTED)<-c("logWages","Data","Labor supply")


WAGESFATHER10=rbind(WAGESFATHER10PREDICTED,WAGESFATHER10OBSERVED)
p<-ggplot(WAGESFATHER10, aes(logWages,fill=Data,colour=Data)) +
  geom_density(adjust=BW,alpha=0.3)+labs(title="2010")
p<-p+xlim(c(5,20))
p<-p+labs(title="Men",x="Log-wages", y="Density")

p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.title=element_text(size=rel(0)))
p
dev.set()
png(file="Fwages2010.png")
p
dev.off()

#------------------------------
#Table of mean and sd of wages
#------------------------------

DT=2 #Digits of precision in table
#I.1 Fathers 2010
Table1<-matrix(0,2,2)
rownames(Table1)<-c("Mean","SD")
colnames(Table1)<-c("Predicted","Observed")
Table1[1,1]<-round(mean(WAGESFATHER10PREDICTED$logWages),2)
Table1[1,2]<-round(mean(WAGESFATHER10OBSERVED$logWages),2)
Table1[2,1]<-round(sqrt(var(WAGESFATHER10PREDICTED$logWages)),2)
Table1[2,2]<-round(sqrt(var(WAGESFATHER10OBSERVED$logWages)),2)
Table1LATEX<-xtable(Table1,caption="2010",digits=DT, align="lll",table.placement="H",label="tab:ModelfitWagesFather2010")
write(print(Table1LATEX,include.rownames=(TRUE), align="lcc", table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="ModelfitWagesFather2010.tex")
#Nocaption
Table1LATEX<-xtable(Table1,digits=DT,table.placement="H", align="lcc",label="tab:ModelfitWagesFather2010")
write(print(Table1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitWagesFather2010NOCAPTION.tex")


#I.1 Fathers 2012
Table1<-matrix(0,2,2)
rownames(Table1)<-c("Mean","SD")
colnames(Table1)<-c("Predicted","Observed")
Table1[1,1]<-round(mean(WAGESFATHER12PREDICTED$logWages),2)
Table1[1,2]<-round(mean(WAGESFATHER12OBSERVED$logWages),2)
Table1[2,1]<-round(sqrt(var(WAGESFATHER12PREDICTED$logWages)),2)
Table1[2,2]<-round(sqrt(var(WAGESFATHER12OBSERVED$logWages)),2)
Table1LATEX<-xtable(Table1,caption="2010",digits=DT, align="lll",table.placement="H",label="tab:ModelfitWagesFather2012")
write(print(Table1LATEX,include.rownames=(TRUE), align="lcc", table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="ModelfitWagesFather2012.tex")
#Nocaption
Table1LATEX<-xtable(Table1,digits=DT,table.placement="H", align="lcc",label="tab:ModelfitWagesFather2012")
write(print(Table1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitWagesFather2012NOCAPTION.tex")


#I.1 Mothers 2010
Table1<-matrix(0,2,2)
rownames(Table1)<-c("Mean","SD")
colnames(Table1)<-c("Predicted","Observed")
Table1[1,1]<-round(mean(WAGESMOTHER10PREDICTED$logWages),2)
Table1[1,2]<-round(mean(WAGESMOTHER10OBSERVED$logWages),2)
Table1[2,1]<-round(sqrt(var(WAGESMOTHER10PREDICTED$logWages)),2)
Table1[2,2]<-round(sqrt(var(WAGESMOTHER10OBSERVED$logWages)),2)
write(print(Table1LATEX,include.rownames=(TRUE), align="lcc", table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="ModelfitWagesMother2010.tex")
#Nocaption
Table1LATEX<-xtable(Table1,digits=DT,table.placement="H", align="lcc",label="tab:ModelfitWagesMother2010")
write(print(Table1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitWagesMother2010NOCAPTION.tex")


#I.1 Mothers 2012
Table1<-matrix(0,2,2)
rownames(Table1)<-c("Mean","SD")
colnames(Table1)<-c("Predicted","Observed")
Table1[1,1]<-round(mean(WAGESMOTHER12PREDICTED$logWages),2)
Table1[1,2]<-round(mean(WAGESMOTHER12OBSERVED$logWages),2)
Table1[2,1]<-round(sqrt(var(WAGESMOTHER12PREDICTED$logWages)),2)
Table1[2,2]<-round(sqrt(var(WAGESMOTHER12OBSERVED$logWages)),2)
write(print(Table1LATEX,include.rownames=(TRUE), align="lcc", table.placement="H", sanitize.text.function=function(x){x},caption.placement = "top"),file="ModelfitWagesMother2012.tex")
#Nocaption
Table1LATEX<-xtable(Table1,digits=DT,table.placement="H", align="lcc",label="tab:ModelfitWagesMother2012")
write(print(Table1LATEX,include.rownames=(TRUE),  table.placement="H", sanitize.text.function=function(x){x}),file="ModelfitWagesMother2012NOCAPTION.tex")





#Data analysis end
MotherWageAnalysis=data.frame(PREDWAGEMOTHER10,PREDWAGEMOTHER12,data.Mfraclabor10,data.Mfraclabor12,data.Myrschool12,data.Mwage12,data.Mwage10)

MPredmean10=mean(subset(MotherWageAnalysis,data.Mfraclabor10==1)$PREDWAGEMOTHER10)/500
MObsdmean10=mean(subset(MotherWageAnalysis,data.Mfraclabor10==1)$data.Mwage10)/500

MPredmean12=(mean(subset(MotherWageAnalysis,data.Mfraclabor12==1)$PREDWAGEMOTHER12))/500
MObsdmean12=(mean(subset(MotherWageAnalysis,data.Mfraclabor12==1)$data.Mwage12))/500

MPredmean10LHS=(mean(subset(MotherWageAnalysis,data.Mfraclabor10==1 & data.Myrschool12<12)$PREDWAGEMOTHER10))/500
MObsdmean10LHS=(mean(subset(MotherWageAnalysis,data.Mfraclabor10==1 & data.Myrschool12<12)$data.Mwage10))/500

MPredmean12LHS=log(mean(subset(MotherWageAnalysis,data.Mfraclabor12==1 & data.Myrschool12<12 & data.Myrschool12>=2)$PREDWAGEMOTHER12))
MObsdmean12LHS=log(mean(subset(MotherWageAnalysis,data.Mfraclabor12==1 & data.Myrschool12<12 & data.Myrschool12>=2)$data.Mwage12))

MPredmean12LHS=log(mean(subset(MotherWageAnalysis,data.Mfraclabor12==1 & data.Myrschool12==12)$PREDWAGEMOTHER12))
MObsdmean12LHS=log(mean(subset(MotherWageAnalysis,data.Mfraclabor12==1 & data.Myrschool12==12 )$data.Mwage12))


FatherWagesAnalysis=cbind(PREDWAGEFATHER10,PREDWAGEMOTHER)

setwd(direverything)





