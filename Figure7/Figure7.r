#------------#
#Housekeeping#
#------------#                                                                                                                  





#source('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL67/BEHAVIORALMODELFIT.R', echo=FALSE)

rm(list=ls(all=TRUE))


#Fraction that the subsidy is going to reduce the price of investments

#Size of the subsidy in the first counterfactuals: 3567
EXPENDITURE1=5764*950*0.2  #It is distributed in two years This is for each year
#If it is the same cost: 2841.54*950*0.2
EXPENDITURE1=3252.930*950*0.2
#THE EXPENDITURE PER FAMILY is called subpf
subpf=3252.930


#===================================================#
#Amount of money spent in each policy counterfactual#
#===================================================#


#Total amount of money spent in Counterfactual1 (In Chilean Pesos):
EXPENDITURE1=subpf*950*0.2

#Expenditure in the second counterfactual is exactly the same as the first one. So no need to compute anything here. 
EXPENDITURE2=EXPENDITURE1


#EXPENDITURE3 will store the amount of money spent in the childcare subsidy. It will be given as output later. 
EXPENDITURE3=0

#Expenditure in the fourth counterfactual is exactly the same as in the fourth one. 
EXPENDITURE4=EXPENDITURE1


#SUBSIDY FOR THIRD COUNTERFACTUAL: Free, price of childcare becomes free for people in the lowest income quintile

#SUBSIDY FOR FOURTH COUNTERFACTUAL:
#COMPUTE THE PRICE OF THE subsidy. Cinv12PREDCOUNTER4 gives the 
#expenditure in investment. Of this, the household pays
#1/TIMESUB and the government pays the remaining 1-1/Timesub 
#fraction. Only for the beneficiaries.

#IDEALLY, EXPENDITURE4 SHOULD BE EQUAL TO EXPENDITURE1. HOWEVER, IF WE ASSUME THAT NOT ALL OF THIS IS INVESTED IN CHILDREN
#WE NEED TO PUT A LOWER BOUND AND AN UPPER BOUND. AS IT IS FOR THE MOMENT IS APPROXIMATELY HALF OF IT. 
#HOWEVER, WE CAN EVEN APPROXIMATE IT TO A THIRD. 
TIMESUB=100


#Run the necessary functions and libraries
library(ggplot2)
library(gridExtra)
library(xtable)

#Multiplot
source('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/RfunctionsUsed/multiplot.R', echo=TRUE)

#----------------------------------------------------
#Define the parameters of the graphs to be generated#
#----------------------------------------------------
setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/Graphs")

#0. Directory to store and save graphs
initialdir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/Graphs"

#0.1 Directory where the final graphs are going to be saved (combined ones)
finaldir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/Graphs/CombinedResults"

#0.2 Directory where everything is stored
direverything="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72"

#1. Thickness of lines
sizegraphs=2 

#--------------------------#
#Running the rcpp functions#
#--------------------------#


Rcpp::sourceCpp('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/mainRCPP.cpp')
#Rcpp::sourceCpp('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL63/MainParallel.cpp')

#------------------------------------#
#Loading the optimal parameters found#
#------------------------------------#

#Chose which parameters to load
#BEST FIT: run on the amazonn server

#Initial results
#algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL64/PAROPTFOUNDPARALLEL2.csv", sep=",", header=FALSE)


#LATEST FOUND IN OPENMP
algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/PAR28.csv", sep=",", header=FALSE)
algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/PARTESLAFOUND.csv", sep=",", header=FALSE)


paropt<-algo[,1]


#-----------------------------#
#Loading the necessary dataset#
#-----------------------------#

e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/BEHAVIORALWNAMES.csv", quote="")
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

NLINCOMEORIGINAL<-data.Mnlincome12
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
  price=exp(paropt[20])+exp(paropt[325])*(1/(1+data.Hchildcareobs[ii]))
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
      Cinv10PRED[ii]=CinvestmentNWNWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWNWNA_TOG10
    }
    
    #If predicted is NWNWA
    if (maxWELF10==WelfareNWNWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortNWNW_TOG10
      Feffort10PRED[ii]=FeffortNWNW_TOG10
      Cinv10PRED[ii]=CinvestmentNWNWA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWNWA_TOG10
    }
    
    
    #If predicted is WNWNA
    if (maxWELF10==WelfareWNWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsWNWNA_TOG10
    }
    
    #If predicted is WNWA
    if (maxWELF10==WelfareWNWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWA_TOG10*price
      Cskills10PRED[ii]=CSkillsWNWA_TOG10
    }
    
    #If predicted is NWWNA
    if (maxWELF10==WelfareNWWNA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWWNA_TOG10
    }
    
    #If predicted is NWWA
    if (maxWELF10==WelfareNWWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWWA_TOG10
    }
    
    
    #If predicted is WWNA
    if (maxWELF10==WelfareWWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsWWNA_TOG10
    }
    
    #If predicted is WWA
    if (maxWELF10==WelfareWWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWA_TOG10*price
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
      Cinv10PRED[ii]=Cinvestment10NWNA*price
      Cskills10PRED[ii]=CSkills10NWNA
    }else if ((maxWELF10==MutilityNWA10) ){
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=Meffort10NW
      Cinv10PRED[ii]=Cinvestment10NWA*price
      Cskills10PRED[ii]=CSkills10NWA
    }else if ((maxWELF10==MutilityWNA10) ){
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=Meffort10W
      Cinv10PRED[ii]=Cinvestment10WNA*price
      Cskills10PRED[ii]=CSkills10WNA
    }else if ((maxWELF10==MutilityWA10) ){
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=Meffort10W
      Cinv10PRED[ii]=Cinvestment10WA*price
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
                                   CinvestmentNWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    
    CSkillsWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG12,MeffortWNW_TOG12,
                                  CinvestmentWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    
    CSkillsNWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG12,MeffortNWW_TOG12,
                                  CinvestmentNWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                 ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG12,MeffortWW_TOG12,
                                 CinvestmentWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
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
      Cinv12PRED[ii]=CinvestmentNWNW_TOG12*price
      Cskills12PRED[ii]=CSkillsNWNW_TOG12
    }
    if (maxWELF12==WelfareWNW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=0
      Meffort12PRED[ii]=MeffortWNW_TOG12
      Feffort12PRED[ii]=FeffortWNW_TOG12
      Cinv12PRED[ii]=CinvestmentWNW_TOG12*price
      Cskills12PRED[ii]=CSkillsWNW_TOG12
    }
    if (maxWELF12==WelfareNWW12){
      Ffraclabor12PRED[ii]=0
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortNWW_TOG12
      Feffort12PRED[ii]=FeffortNWW_TOG12
      Cinv12PRED[ii]=CinvestmentNWW_TOG12*price
      Cskills12PRED[ii]=CSkillsNWW_TOG12
    }
    if (maxWELF12==WelfareWW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortWW_TOG12
      Feffort12PRED[ii]=FeffortWW_TOG12
      Cinv12PRED[ii]=CinvestmentWW_TOG12*price
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
      Cinv12PRED[ii]=Cinvestment12NW*price
      Cskills12PRED[ii]=CSkills12NW
    }else {
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=Meffort12W
      Cinv12PRED[ii]=Cinvestment12W*price
      Cskills12PRED[ii]=CSkills12W
    }
    
  }#End if single mother in 2012
  
}

Cinv12PREDORIG<-Cinv12PRED
Cskills12PREDORIG<-Cskills12PRED
Cskills10PREDORIG<-Cskills10PRED
Meffort12ORIG<-Meffort12PRED
Feffort12ORIG<-Feffort12PRED
Meffort10ORIG<-Meffort10PRED
Feffort10ORIG<-Feffort10PRED
Cinv10PREDORIG<-Cinv10PRED
Cchildcare10ORIG<-predchildcare10
Mfraclabor12PREDORIG<-Mfraclabor12PRED
Ffraclabor12PREDORIG<-Ffraclabor12PRED
Mfraclabor10PREDORIG<-Mfraclabor10PRED
Ffraclabor10PREDORIG<-Ffraclabor10PRED
#Mmupred12PREDORIG<-Mmupred
Mfraclabor12OBSERVED<-data.Mfraclabor12
Ffraclabor12OBSERVED<-data.Ffraclabor12
Mfraclabor10OBSERVED<-data.Mfraclabor10
Ffraclabor10OBSERVED<-data.Ffraclabor10
Cchildcare10OBSERVED<-data.Cchildcare10





























































#------------------------------------------------------------------#
#COUNTERFACTUAL 1
#------------------------------------------------------------------#



#The vector BEN will identify who is a beneficiary and who is not
BEN<-seq(1,950)


#Calculating who is in the lowest 20% of the income distribution
TOTINCOME<-data.Mnlincome12+data.Fnlincome12+data.Mwage12*data.Mfraclabor12+data.Fwage12*data.Ffraclabor12

#The benefit is given in 2012 and let's transform it to benefits in 2012. 
THRESHOLD<-quantile(TOTINCOME,probs=c(0.2))
BEN<-1*(TOTINCOME<THRESHOLD)

#Total money spent is divided in two periods for everybody. 


#-----------------------------#
#Loading the necessary dataset#
#-----------------------------#

LOOPLENGTH<-100


SKILLSCOUNTERFACTUAL1<-matrix(0,ncol=LOOPLENGTH,nrow=950)

BENEFIT<-seq(0.0,0.99,0.01)*5000000
AMOUNTSPENTX1<-BENEFIT
#jjloop includes all the total money spent in people. 
#it<- iterator for the loopsi n case the money does not go from 1 to 1

for(jjloop in 1:LOOPLENGTH){
  #Size of the benefit is jjloop/190
  Benefit<-BENEFIT[jjloop]/(190*2)*BEN

e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/BEHAVIORALWNAMES.csv", quote="")
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
data.Mnlincome12=data.matrix(e$Mnlincome12,rownames.force=NA)+Benefit
data.Fnlincome12=data.matrix(e$Fnlincome12,rownames.force=NA)
data.Mnlincome10=data.matrix(e$Mnlincome10,rownames.force=NA)+Benefit
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
data.HDens5=e$HDens5
#MONEY ENETERED with subsidy. This is done to compute the total amount of money spent in the subsidy
NLINCOMECOUNTERFACTUAL1<-data.Mnlincome12


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

#Predicted mmu:
Mmupred<-rep(0,SIZE)


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
  price=exp(paropt[20])+exp(paropt[325])*(1/(1+data.Hchildcareobs[ii]))
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
      Cinv10PRED[ii]=CinvestmentNWNWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWNWNA_TOG10
    }
    
    #If predicted is NWNWA
    if (maxWELF10==WelfareNWNWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortNWNW_TOG10
      Feffort10PRED[ii]=FeffortNWNW_TOG10
      Cinv10PRED[ii]=CinvestmentNWNWA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWNWA_TOG10
    }
    
    
    #If predicted is WNWNA
    if (maxWELF10==WelfareWNWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsWNWNA_TOG10
    }
    
    #If predicted is WNWA
    if (maxWELF10==WelfareWNWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWA_TOG10*price
      Cskills10PRED[ii]=CSkillsWNWA_TOG10
    }
    
    #If predicted is NWWNA
    if (maxWELF10==WelfareNWWNA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWWNA_TOG10
    }
    
    #If predicted is NWWA
    if (maxWELF10==WelfareNWWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWWA_TOG10
    }
    
    
    #If predicted is WWNA
    if (maxWELF10==WelfareWWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsWWNA_TOG10
    }
    
    #If predicted is WWA
    if (maxWELF10==WelfareWWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWA_TOG10*price
      Cskills10PRED[ii]=CSkillsWWA_TOG10
    }
    
  }#End if they live together
  
  
  
  
  #Now, in the 2012
  #1. If they live together
  if ( (data.Cliveswithfather12[ii]==1)  && (data.Cliveswithmother12[ii]==1)){
    #0. Getting the predicted level of mmu:
    mmu12=F_mmu(llambda0,llambda1,llambda2,llambda3,llambda4,llambda5,llambda6,llambda7,llambda8,Fwagepred12[ii],
                Mwagepred12[ii],data.Fnlincome12[ii],data.Mnlincome12[ii],0,
                0.2,0.8,data.Fage12[ii],data.Mage12[ii],data.Fyrschool12[ii],data.Myrschool12[ii],data.FMRATIO[ii],
                data.Unemployment[ii],data.Wageratio[ii],data.Distance[ii])
    Mmupred[ii]=mmu12
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
                                   CinvestmentNWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG12,MeffortWNW_TOG12,
                                  CinvestmentWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsNWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG12,MeffortNWW_TOG12,
                                  CinvestmentNWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                 ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG12,MeffortWW_TOG12,
                                 CinvestmentWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
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
      Cinv12PRED[ii]=CinvestmentNWNW_TOG12*price
      Cskills12PRED[ii]=CSkillsNWNW_TOG12
    }
    if (maxWELF12==WelfareWNW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=0
      Meffort12PRED[ii]=MeffortWNW_TOG12
      Feffort12PRED[ii]=FeffortWNW_TOG12
      Cinv12PRED[ii]=CinvestmentWNW_TOG12*price
      Cskills12PRED[ii]=CSkillsWNW_TOG12
    }
    if (maxWELF12==WelfareNWW12){
      Ffraclabor12PRED[ii]=0
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortNWW_TOG12
      Feffort12PRED[ii]=FeffortNWW_TOG12
      Cinv12PRED[ii]=CinvestmentNWW_TOG12*price
      Cskills12PRED[ii]=CSkillsNWW_TOG12
    }
    if (maxWELF12==WelfareWW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortWW_TOG12
      Feffort12PRED[ii]=FeffortWW_TOG12
      Cinv12PRED[ii]=CinvestmentWW_TOG12*price
      Cskills12PRED[ii]=CSkillsWW_TOG12
    }
    
  }#End if they live together 2012
  
  
}



Meffort12PREDCOUNTER1<-Meffort12PRED
Feffort12PREDCOUNTER1<-Feffort12PRED
Meffort10PREDCOUNTER1<-Meffort10PRED
Feffort10PREDCOUNTER1<-Feffort10PRED
Cinv10PREDCOUNTER1<-Cinv10PRED
Cinv12PREDCOUNTER1<-Cinv12PRED
Cskills10PREDCOUNTER1<-Cskills10PRED
Cskills12PREDCOUNTER1<-Cskills12PRED
Mfraclabor12PREDCOUNTER1<-Mfraclabor12PRED
Ffraclabor12PREDCOUNTER1<-Ffraclabor12PRED
Mfraclabor10PREDCOUNTER1<-Mfraclabor10PRED
Ffraclabor10PREDCOUNTER1<-Ffraclabor10PRED
Mmupred12PREDCOUNTER1<-Mmupred
Cchildcare10PREDCOUNTER1<-predchildcare10
SKILLSCOUNTERFACTUAL1[1:950,jjloop]<-Cskills12PREDCOUNTER1
}


















#------------------------------------------------------------------#
#COUNTERFACTUAL 2
#------------------------------------------------------------------#



#The vector BEN will identify who is a beneficiary and who is not
BEN<-seq(1,950)


#Calculating who is in the lowest 20% of the income distribution
TOTINCOME<-data.Mnlincome12+data.Fnlincome12+data.Mwage12*data.Mfraclabor12+data.Fwage12*data.Ffraclabor12

#The benefit is given in 2012 and let's transform it to benefits in 2012. 
THRESHOLD<-quantile(TOTINCOME,probs=c(0.2))
BEN<-1*(TOTINCOME<THRESHOLD)

#Total money spent is divided in two periods for everybody. 


#-----------------------------#
#Loading the necessary dataset#
#-----------------------------#

LOOPLENGTH<-100


SKILLSCOUNTERFACTUAL2<-matrix(0,ncol=LOOPLENGTH,nrow=950)

BENEFIT<-seq(0.0,0.99,0.01)*5000000
AMOUNTSPENTX2<-BENEFIT
#jjloop includes all the total money spent in people. 
#it<- iterator for the loopsi n case the money does not go from 1 to 1

for(jjloop in 1:LOOPLENGTH){
  #Size of the benefit is jjloop/190
  Benefit<-BENEFIT[jjloop]/(190*2)*BEN
  
  e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/BEHAVIORALWNAMES.csv", quote="")
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
  data.Fnlincome12=data.matrix(e$Fnlincome12,rownames.force=NA)+Benefit
  data.Mnlincome10=data.matrix(e$Mnlincome10,rownames.force=NA)
  data.Fnlincome10=data.matrix(e$Fnlincome10,rownames.force=NA)+Benefit
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
  data.HDens5=e$HDens5
  #MONEY ENETERED with subsidy. This is done to compute the total amount of money spent in the subsidy
  NLINCOMECOUNTERFACTUAL1<-data.Mnlincome12
  
  
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
  
  #Predicted mmu:
  Mmupred<-rep(0,SIZE)
  
  
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
    price=exp(paropt[20])+exp(paropt[325])*(1/(1+data.Hchildcareobs[ii]))
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
        Cinv10PRED[ii]=CinvestmentNWNWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWNWNA_TOG10
      }
      
      #If predicted is NWNWA
      if (maxWELF10==WelfareNWNWA10){
        Ffraclabor10PRED[ii]=0
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortNWNW_TOG10
        Feffort10PRED[ii]=FeffortNWNW_TOG10
        Cinv10PRED[ii]=CinvestmentNWNWA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWNWA_TOG10
      }
      
      
      #If predicted is WNWNA
      if (maxWELF10==WelfareWNWNA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=0
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortWNW_TOG10
        Cinv10PRED[ii]=CinvestmentWNWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsWNWNA_TOG10
      }
      
      #If predicted is WNWA
      if (maxWELF10==WelfareWNWA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortWNW_TOG10
        Cinv10PRED[ii]=CinvestmentWNWA_TOG10*price
        Cskills10PRED[ii]=CSkillsWNWA_TOG10
      }
      
      #If predicted is NWWNA
      if (maxWELF10==WelfareNWWNA10){
        Ffraclabor10PRED[ii]=0
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=0
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortNWW_TOG10
        Cinv10PRED[ii]=CinvestmentNWWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWWNA_TOG10
      }
      
      #If predicted is NWWA
      if (maxWELF10==WelfareNWWA10){
        Ffraclabor10PRED[ii]=0
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortNWW_TOG10
        Cinv10PRED[ii]=CinvestmentNWWA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWWA_TOG10
      }
      
      
      #If predicted is WWNA
      if (maxWELF10==WelfareWWNA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=0
        Meffort10PRED[ii]=MeffortWW_TOG10
        Feffort10PRED[ii]=FeffortWW_TOG10
        Cinv10PRED[ii]=CinvestmentWWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsWWNA_TOG10
      }
      
      #If predicted is WWA
      if (maxWELF10==WelfareWWA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortWW_TOG10
        Feffort10PRED[ii]=FeffortWW_TOG10
        Cinv10PRED[ii]=CinvestmentWWA_TOG10*price
        Cskills10PRED[ii]=CSkillsWWA_TOG10
      }
      
    }#End if they live together
    
    
    
    
    #Now, in the 2012
    #1. If they live together
    if ( (data.Cliveswithfather12[ii]==1)  && (data.Cliveswithmother12[ii]==1)){
      #0. Getting the predicted level of mmu:
      mmu12=F_mmu(llambda0,llambda1,llambda2,llambda3,llambda4,llambda5,llambda6,llambda7,llambda8,Fwagepred12[ii],
                  Mwagepred12[ii],data.Fnlincome12[ii],data.Mnlincome12[ii],0,
                  0.2,0.8,data.Fage12[ii],data.Mage12[ii],data.Fyrschool12[ii],data.Myrschool12[ii],data.FMRATIO[ii],
                  data.Unemployment[ii],data.Wageratio[ii],data.Distance[ii])
      Mmupred[ii]=mmu12
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
                                     CinvestmentNWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      CSkillsWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG12,MeffortWNW_TOG12,
                                    CinvestmentWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      CSkillsNWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG12,MeffortNWW_TOG12,
                                    CinvestmentNWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      CSkillsWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                   ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG12,MeffortWW_TOG12,
                                   CinvestmentWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
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
        Cinv12PRED[ii]=CinvestmentNWNW_TOG12*price
        Cskills12PRED[ii]=CSkillsNWNW_TOG12
      }
      if (maxWELF12==WelfareWNW12){
        Ffraclabor12PRED[ii]=1
        Mfraclabor12PRED[ii]=0
        Meffort12PRED[ii]=MeffortWNW_TOG12
        Feffort12PRED[ii]=FeffortWNW_TOG12
        Cinv12PRED[ii]=CinvestmentWNW_TOG12*price
        Cskills12PRED[ii]=CSkillsWNW_TOG12
      }
      if (maxWELF12==WelfareNWW12){
        Ffraclabor12PRED[ii]=0
        Mfraclabor12PRED[ii]=1
        Meffort12PRED[ii]=MeffortNWW_TOG12
        Feffort12PRED[ii]=FeffortNWW_TOG12
        Cinv12PRED[ii]=CinvestmentNWW_TOG12*price
        Cskills12PRED[ii]=CSkillsNWW_TOG12
      }
      if (maxWELF12==WelfareWW12){
        Ffraclabor12PRED[ii]=1
        Mfraclabor12PRED[ii]=1
        Meffort12PRED[ii]=MeffortWW_TOG12
        Feffort12PRED[ii]=FeffortWW_TOG12
        Cinv12PRED[ii]=CinvestmentWW_TOG12*price
        Cskills12PRED[ii]=CSkillsWW_TOG12
      }
      
    }#End if they live together 2012
    
    
  }
  
  
  
  Meffort12PREDCOUNTER2<-Meffort12PRED
  Feffort12PREDCOUNTER2<-Feffort12PRED
  Meffort10PREDCOUNTER2<-Meffort10PRED
  Feffort10PREDCOUNTER2<-Feffort10PRED
  Cinv10PREDCOUNTER2<-Cinv10PRED
  Cinv12PREDCOUNTER2<-Cinv12PRED
  Cskills10PREDCOUNTER2<-Cskills10PRED
  Cskills12PREDCOUNTER2<-Cskills12PRED
  Mfraclabor12PREDCOUNTER2<-Mfraclabor12PRED
  Ffraclabor12PREDCOUNTER2<-Ffraclabor12PRED
  Mfraclabor10PREDCOUNTER2<-Mfraclabor10PRED
  Ffraclabor10PREDCOUNTER2<-Ffraclabor10PRED
  Mmupred12PREDCOUNTER2<-Mmupred
  Cchildcare10PREDCOUNTER2<-predchildcare10
  SKILLSCOUNTERFACTUAL2[1:950,jjloop]<-Cskills12PREDCOUNTER2
}














#====================#
#THIRD COUNTERFACTUAL#
#====================#
SKILLSCOUNTERFACTUAL3<-matrix(0,ncol=LOOPLENGTH,nrow=950)

#-----------------------------#
#Loading the necessary dataset#
#-----------------------------#

e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/BEHAVIORALWNAMES.csv", quote="")
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
data.HDens5=e$HDens5

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







#Vector BEN will contain the information for those who are located in the bottom 20% of the income distribution. 


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

#Predicted mmu:
Mmupred<-rep(0,SIZE)


#AMOUNT OF MONEY SPENT IN THE THIRD COUNTERFACTUAL
EXPENDITURE3=0

#Each person has its own price of childcare. This vector will store it
pricechildcarepers=pchildcare0+(1/(1+data.Hchildcareobs))*pchildcare1

#==========================================#
#SETTING UP SUBSIDY: AS PROPORTION OF PRICE#
#==========================================#

SUBSIDY3<-seq(0.00,0.99,0.01)

#Storing the amount that is spent for everyone
AMOUNTSPENT3<-matrix(0,nrow=950,ncol=length(SUBSIDY3))
AMOUNTSPENTX3<-numeric(length(SUBSIDY3))

for (jjloop in 1:length(SUBSIDY3)){

  subchildcare=SUBSIDY3[jjloop]
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
  pricechildcareoriginal=pchildcare0+(1/(1+data.Hchildcareobs[ii]))*pchildcare1
  price=exp(paropt[20])+exp(paropt[325])*(1/(1+data.Hchildcareobs[ii]))
  if(BEN[ii]==1){
    pricechildcare=(1-subchildcare)*pricechildcare
    AMOUNTSPENT3[ii,jjloop]=subchildcare*pricechildcareoriginal
  }
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
      Cinv10PRED[ii]=CinvestmentNWNWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWNWNA_TOG10
    }
    
    #If predicted is NWNWA
    if (maxWELF10==WelfareNWNWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortNWNW_TOG10
      Feffort10PRED[ii]=FeffortNWNW_TOG10
      Cinv10PRED[ii]=CinvestmentNWNWA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWNWA_TOG10
    }
    
    
    #If predicted is WNWNA
    if (maxWELF10==WelfareWNWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsWNWNA_TOG10
    }
    
    #If predicted is WNWA
    if (maxWELF10==WelfareWNWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=0
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortWNW_TOG10
      Cinv10PRED[ii]=CinvestmentWNWA_TOG10*price
      Cskills10PRED[ii]=CSkillsWNWA_TOG10
    }
    
    #If predicted is NWWNA
    if (maxWELF10==WelfareNWWNA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWWNA_TOG10
    }
    
    #If predicted is NWWA
    if (maxWELF10==WelfareNWWA10){
      Ffraclabor10PRED[ii]=0
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWNW_TOG10
      Feffort10PRED[ii]=FeffortNWW_TOG10
      Cinv10PRED[ii]=CinvestmentNWWA_TOG10*price
      Cskills10PRED[ii]=CSkillsNWWA_TOG10
    }
    
    
    #If predicted is WWNA
    if (maxWELF10==WelfareWWNA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=0
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWNA_TOG10*price
      Cskills10PRED[ii]=CSkillsWWNA_TOG10
    }
    
    #If predicted is WWA
    if (maxWELF10==WelfareWWA10){
      Ffraclabor10PRED[ii]=1
      Mfraclabor10PRED[ii]=1
      predchildcare10[ii]=1
      Meffort10PRED[ii]=MeffortWW_TOG10
      Feffort10PRED[ii]=FeffortWW_TOG10
      Cinv10PRED[ii]=CinvestmentWWA_TOG10*price
      Cskills10PRED[ii]=CSkillsWWA_TOG10
    }
    
  }#End if they live together
  
  #Compute the cost of the subsity, which has to be computed only if they actually use it
  EXPENDITURE3=EXPENDITURE3+pricechildcareoriginal*BEN[ii]*predchildcare10[ii]
  
  
  #Now, in the 2012
  #1. If they live together
  if ( (data.Cliveswithfather12[ii]==1)  && (data.Cliveswithmother12[ii]==1)){
    #0. Getting the predicted level of mmu:
    mmu12=F_mmu(llambda0,llambda1,llambda2,llambda3,llambda4,llambda5,llambda6,llambda7,llambda8,Fwagepred12[ii],
                Mwagepred12[ii],data.Fnlincome12[ii],data.Mnlincome12[ii],0,
                0.2,0.8,data.Fage12[ii],data.Mage12[ii],data.Fyrschool12[ii],data.Myrschool12[ii],data.FMRATIO[ii],
                data.Unemployment[ii],data.Wageratio[ii],data.Distance[ii])
    Mmupred[ii]=mmu12
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
                                   CinvestmentNWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG12,MeffortWNW_TOG12,
                                  CinvestmentWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsNWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                  ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG12,MeffortNWW_TOG12,
                                  CinvestmentNWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
    CSkillsWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                 ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG12,MeffortWW_TOG12,
                                 CinvestmentWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
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
      Cinv12PRED[ii]=CinvestmentNWNW_TOG12*price
      Cskills12PRED[ii]=CSkillsNWNW_TOG12
    }
    if (maxWELF12==WelfareWNW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=0
      Meffort12PRED[ii]=MeffortWNW_TOG12
      Feffort12PRED[ii]=FeffortWNW_TOG12
      Cinv12PRED[ii]=CinvestmentWNW_TOG12*price
      Cskills12PRED[ii]=CSkillsWNW_TOG12
    }
    if (maxWELF12==WelfareNWW12){
      Ffraclabor12PRED[ii]=0
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortNWW_TOG12
      Feffort12PRED[ii]=FeffortNWW_TOG12
      Cinv12PRED[ii]=CinvestmentNWW_TOG12*price
      Cskills12PRED[ii]=CSkillsNWW_TOG12
    }
    if (maxWELF12==WelfareWW12){
      Ffraclabor12PRED[ii]=1
      Mfraclabor12PRED[ii]=1
      Meffort12PRED[ii]=MeffortWW_TOG12
      Feffort12PRED[ii]=FeffortWW_TOG12
      Cinv12PRED[ii]=CinvestmentWW_TOG12*price
      Cskills12PRED[ii]=CSkillsWW_TOG12
    }
    
  }#End if they live together 2012
  
  
  
}

#Computing expenditure in subsidy:

Meffort12PREDCOUNTER3<-Meffort12PRED
Feffort12PREDCOUNTER3<-Feffort12PRED
Meffort10PREDCOUNTER3<-Meffort10PRED
Feffort10PREDCOUNTER3<-Feffort10PRED
Cinv10PREDCOUNTER3<-Cinv10PRED
Cinv12PREDCOUNTER3<-Cinv12PRED
Cskills12PREDCOUNTER3<-Cskills12PRED
Mfraclabor12PREDCOUNTER3<-Mfraclabor12PRED
Ffraclabor12PREDCOUNTER3<-Ffraclabor12PRED
Mfraclabor10PREDCOUNTER3<-Mfraclabor10PRED
Ffraclabor10PREDCOUNTER3<-Ffraclabor10PRED
Mmupred12PREDCOUNTER3<-Mmupred
Cchildcare10PREDCOUNTER3<-predchildcare10
SKILLSCOUNTERFACTUAL3[1:950,jjloop]<-Cskills12PREDCOUNTER3
AMOUNTSPENTX3[jjloop]<-sum(AMOUNTSPENT3[1:950,jjloop])
}










































































  
  #=====================#
  #FOURTH COUNTERFACTUAL#
  #=====================#
  BEN4<-seq(0,0.99,0.01)
  SIZE4C<-length(BEN4)
  EXPENDITURE4TOTAL<-numeric(SIZE4C)
  SKILLSCOUNTERFACTUAL4<-matrix(0,nrow=950,ncol=SIZE4C)
  for (jjloop in 1:SIZE4C){
    #Proportion of price subsidized<-
    
  
  #-----------------------------#
  #Loading the necessary dataset#
  #-----------------------------#
  
  e <- read.csv("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/BEHAVIORALWNAMES.csv", quote="")
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
  data.HDens5=e$HDens5
  
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
  
  
  
  
  #Setting up the subsidy
  
  moneyspent<-sum(NLINCOMECOUNTERFACTUAL1-NLINCOMEORIGINAL)
  
  #The vector BEN will identify who is a beneficiary and who is not
  BEN<-seq(1,950)
  
  
  #Calculating who is in the lowest 20% of the income distribution
  TOTINCOME<-data.Mnlincome12+data.Fnlincome12+data.Mwage12*data.Mfraclabor12+data.Fwage12*data.Ffraclabor12
  
  #The benefit is given in 2012 and let's transform it to benefits in 2012. 
  THRESHOLD<-quantile(TOTINCOME,probs=c(0.2))
  BEN<-1*(TOTINCOME<THRESHOLD)
  
  #This will store the total amount of money spent on the subsidy
  SUBSIDYTOTAL<-seq(1,950)*0
  subsidy<-BEN4[jjloop] #(20% subsidy for beneficiaries)
  EXPENDITURE4PC<-numeric(950)
  
  #Vector BEN will contain the information for those who are located in the bottom 20% of the income distribution. 
  
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
  
  #Predicted mmu:
  Mmupred<-rep(0,SIZE)
  
  
  
  
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
    price=exp(paropt[20])+exp(paropt[325])*(1/(1+data.Hchildcareobs[ii]))
    subsidizedprice=price
    subsidyinvestment=0
    if (BEN[ii]==1){
      subsidizedprice=price*(1-subsidy)
    }
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
                                            data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentNWNWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                           aalpha2f10,aalpha2m10,mmu,0,0,Fwagepred10[ii],Mwagepred10[ii],
                                           data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentNWWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                           aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                           data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentNWWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                          aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                          data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentNWWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                           aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                           data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentNWWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                          aalpha2f10,aalpha2m10,mmu,0,1,Fwagepred10[ii],Mwagepred10[ii],
                                          data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentWNWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                           aalpha2f10,aalpha2m10,mmu,1,0,Fwagepred10[ii],Mwagepred10[ii],
                                           data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentWNWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                          aalpha2f10,aalpha2m10,mmu,1,0,Fwagepred10[ii],Mwagepred10[ii],
                                          data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,subsidizedprice)
      
      
      CinvestmentWWNA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                          aalpha2f10,aalpha2m10,mmu,1,1,Fwagepred10[ii],Mwagepred10[ii],
                                          data.Fnlincome10[ii],data.Mnlincome10[ii],ttheta0,ttheta1,subsidizedprice)
      
      CinvestmentWWA_TOG10=F_invcouple10(aalpha1f10,aalpha1m10,aalpha2f,aalpha2m,
                                         aalpha2f10,aalpha2m10,mmu,1,1,Fwagepred10[ii],Mwagepred10[ii],
                                         data.Fnlincome10[ii],data.Mnlincome10[ii]-pricechildcare,ttheta0,ttheta1,subsidizedprice)
      
      
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
                                                   aalpha2m10,CinvestmentNWNWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionNWNWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                   aalpha2m10,CinvestmentNWNWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      FConsumptionNWNWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                  aalpha2m10,CinvestmentNWNWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionNWNWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                  aalpha2m10,CinvestmentNWNWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      
      FConsumptionWNWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                  aalpha2m10,CinvestmentWNWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionWNWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                  aalpha2m10,CinvestmentWNWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      FConsumptionWNWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentWNWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionWNWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentWNWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      
      
      
      FConsumptionNWWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                  aalpha2m10,CinvestmentNWWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionNWWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                  aalpha2m10,CinvestmentNWWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      FConsumptionNWWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentNWWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionNWWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentNWWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      
      FConsumptionWWNA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentWWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionWWNA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                 aalpha2m10,CinvestmentWWNA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
      FConsumptionWWA_TOG10=F_consumption_TOG10(aalpha1f10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentWWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      MConsumptionWWA_TOG10=M_consumption_TOG10(aalpha1m10,aalpha2f,aalpha2m,aalpha2f10,
                                                aalpha2m10,CinvestmentWWA_TOG10,ttheta0,ttheta1,mmu,subsidizedprice)
      
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
        Cinv10PRED[ii]=CinvestmentNWNWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWNWNA_TOG10
      }
      
      #If predicted is NWNWA
      if (maxWELF10==WelfareNWNWA10){
        Ffraclabor10PRED[ii]=0
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortNWNW_TOG10
        Feffort10PRED[ii]=FeffortNWNW_TOG10
        Cinv10PRED[ii]=CinvestmentNWNWA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWNWA_TOG10
      }
      
      
      #If predicted is WNWNA
      if (maxWELF10==WelfareWNWNA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=0
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortWNW_TOG10
        Cinv10PRED[ii]=CinvestmentWNWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsWNWNA_TOG10
      }
      
      #If predicted is WNWA
      if (maxWELF10==WelfareWNWA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortWNW_TOG10
        Cinv10PRED[ii]=CinvestmentWNWA_TOG10*price
        Cskills10PRED[ii]=CSkillsWNWA_TOG10
      }
      
      #If predicted is NWWNA
      if (maxWELF10==WelfareNWWNA10){
        Ffraclabor10PRED[ii]=0
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=0
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortNWW_TOG10
        Cinv10PRED[ii]=CinvestmentNWWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWWNA_TOG10
      }
      
      #If predicted is NWWA
      if (maxWELF10==WelfareNWWA10){
        Ffraclabor10PRED[ii]=0
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortWNW_TOG10
        Feffort10PRED[ii]=FeffortNWW_TOG10
        Cinv10PRED[ii]=CinvestmentNWWA_TOG10*price
        Cskills10PRED[ii]=CSkillsNWWA_TOG10
      }
      
      
      #If predicted is WWNA
      if (maxWELF10==WelfareWWNA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=0
        Meffort10PRED[ii]=MeffortWW_TOG10
        Feffort10PRED[ii]=FeffortWW_TOG10
        Cinv10PRED[ii]=CinvestmentWWNA_TOG10*price
        Cskills10PRED[ii]=CSkillsWWNA_TOG10
      }
      
      #If predicted is WWA
      if (maxWELF10==WelfareWWA10){
        Ffraclabor10PRED[ii]=1
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=1
        Meffort10PRED[ii]=MeffortWW_TOG10
        Feffort10PRED[ii]=FeffortWW_TOG10
        Cinv10PRED[ii]=CinvestmentWWA_TOG10*price
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
                                       Mwagepred10[ii],data.Mnlincome10[ii],0,subsidizedprice)
      
      Cinvestment10NWA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                      Mwagepred10[ii],data.Mnlincome10[ii]-pricechildcare,0,subsidizedprice)
      
      
      Cinvestment10WNA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                      Mwagepred10[ii],data.Mnlincome10[ii],1,subsidizedprice)
      
      Cinvestment10WA=F_investment10(aalpha1m10,aalpha2m,aalpha2m10,ttheta1,ttheta0,
                                     Mwagepred10[ii],data.Mnlincome10[ii]-pricechildcare,1,subsidizedprice)
      
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
        Cinv10PRED[ii]=Cinvestment10NWNA*price
        Cskills10PRED[ii]=CSkills10NWNA
      }else if ((maxWELF10==MutilityNWA10) ){
        Mfraclabor10PRED[ii]=0
        predchildcare10[ii]=1
        Meffort10PRED[ii]=Meffort10NW
        Cinv10PRED[ii]=Cinvestment10NWA*price
        Cskills10PRED[ii]=CSkills10NWA
      }else if ((maxWELF10==MutilityWNA10) ){
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=0
        Meffort10PRED[ii]=Meffort10W
        Cinv10PRED[ii]=Cinvestment10WNA*price
        Cskills10PRED[ii]=CSkills10WNA
      }else if ((maxWELF10==MutilityWA10) ){
        Mfraclabor10PRED[ii]=1
        predchildcare10[ii]=1
        Meffort10PRED[ii]=Meffort10W
        Cinv10PRED[ii]=Cinvestment10WA*price
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
                                        data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,subsidizedprice)
      
      CinvestmentWNW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                       mmu12,1,0,Fwagepred12[ii],Mwagepred12[ii],
                                       data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,subsidizedprice)
      CinvestmentNWW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                       mmu12,0,1,Fwagepred12[ii],Mwagepred12[ii],
                                       data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,subsidizedprice)
      CinvestmentWW_TOG12=F_invcouple(aalpha1m,aalpha1f,aalpha2m,aalpha2f,
                                      mmu12,1,1,Fwagepred12[ii],Mwagepred12[ii],
                                      data.Fnlincome12[ii],data.Mnlincome12[ii],ttheta1,subsidizedprice)
      
      
      #3. Predicted levels of skills
      CSkillsNWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                     ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWNW_TOG12,MeffortNWNW_TOG12,
                                     CinvestmentNWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      CSkillsWNW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWNW_TOG12,MeffortWNW_TOG12,
                                    CinvestmentWNW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      CSkillsNWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                    ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortNWW_TOG12,MeffortNWW_TOG12,
                                    CinvestmentNWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      CSkillsWW_TOG12=F_predskills(ddelta0,ddelta1,ddelta2,ddelta3_12,ddelta4,data.Cedad_meses12[ii],ttheta0,
                                   ttheta1,ttheta2,pphi,ggammaf,ggammam,FeffortWW_TOG12,MeffortWW_TOG12,
                                   CinvestmentWW_TOG12,Cskills10PRED[ii],data.Cchildcare12[ii],data.Ccareskills[ii],data.Hmemberstotal12[ii])
      #4. Getting consumption levels
      FConsumptionNWNW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                               CinvestmentNWNW_TOG12,ttheta1,mmu12,subsidizedprice)
      MConsumptionNWNW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                               CinvestmentNWNW_TOG12,ttheta1,mmu12,subsidizedprice)
      
      FConsumptionWNW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                              CinvestmentWNW_TOG12,ttheta1,mmu12,subsidizedprice)
      MConsumptionWNW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                              CinvestmentWNW_TOG12,ttheta1,mmu12,subsidizedprice)
      
      FConsumptionNWW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                              CinvestmentNWW_TOG12,ttheta1,mmu12,subsidizedprice)
      MConsumptionNWW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                              CinvestmentNWW_TOG12,ttheta1,mmu12,subsidizedprice)
      
      FConsumptionWW_TOG12=F_consumption_TOG(aalpha1f,aalpha2f,aalpha2m,
                                             CinvestmentWW_TOG12,ttheta1,mmu12,subsidizedprice)
      MConsumptionWW_TOG12=M_consumption_TOG(aalpha1m,aalpha2f,aalpha2m,
                                             CinvestmentWW_TOG12,ttheta1,mmu12,subsidizedprice)
      
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
        Cinv12PRED[ii]=CinvestmentNWNW_TOG12*price
        Cskills12PRED[ii]=CSkillsNWNW_TOG12
      }
      if (maxWELF12==WelfareWNW12){
        Ffraclabor12PRED[ii]=1
        Mfraclabor12PRED[ii]=0
        Meffort12PRED[ii]=MeffortWNW_TOG12
        Feffort12PRED[ii]=FeffortWNW_TOG12
        Cinv12PRED[ii]=CinvestmentWNW_TOG12*price
        Cskills12PRED[ii]=CSkillsWNW_TOG12
      }
      if (maxWELF12==WelfareNWW12){
        Ffraclabor12PRED[ii]=0
        Mfraclabor12PRED[ii]=1
        Meffort12PRED[ii]=MeffortNWW_TOG12
        Feffort12PRED[ii]=FeffortNWW_TOG12
        Cinv12PRED[ii]=CinvestmentNWW_TOG12*price
        Cskills12PRED[ii]=CSkillsNWW_TOG12
      }
      if (maxWELF12==WelfareWW12){
        Ffraclabor12PRED[ii]=1
        Mfraclabor12PRED[ii]=1
        Meffort12PRED[ii]=MeffortWW_TOG12
        Feffort12PRED[ii]=FeffortWW_TOG12
        Cinv12PRED[ii]=CinvestmentWW_TOG12*price
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
        Cinv12PRED[ii]=Cinvestment12NW*price
        Cskills12PRED[ii]=CSkills12NW
      }else {
        Mfraclabor12PRED[ii]=1
        Meffort12PRED[ii]=Meffort12W
        Cinv12PRED[ii]=Cinvestment12W*price
        Cskills12PRED[ii]=CSkills12W
      }
      
    }#End if single mother in 2012
    
    #Units of investment bought Cinv12PREDCOUNTER4/price. 
    #Family payed (Cinv12PREDCOUNTER4)subsidizedprice/price 
    #Governmnent payed the difference (Cinv12PREDCOUNTER4/price)(1-subsidizedprice/price):
    EXPENDITURE4PC[ii]=(Cinv12PRED[ii]/price)*(price-subsidizedprice)+(Cinv10PRED[ii]/price)*(price-subsidizedprice)
    
    
  }
  Meffort12PREDCOUNTER4<-Meffort12PRED
  Feffort12PREDCOUNTER4  <-Feffort12PRED
  Meffort10PREDCOUNTER4<-Meffort10PRED
  Feffort10PREDCOUNTER4<-Feffort10PRED
  Cinv12PREDCOUNTER4<-Cinv12PRED
  Cinv10PREDCOUNTER4<-Cinv10PRED
  Cskills12PREDCOUNTER4<-Cskills12PRED
  Mfraclabor12PREDCOUNTER4<-Mfraclabor12PRED
  Ffraclabor12PREDCOUNTER4<-Ffraclabor12PRED
  Mfraclabor10PREDCOUNTER4<-Mfraclabor10PRED
  Ffraclabor10PREDCOUNTER4<-Ffraclabor10PRED
  Mmupred12PREDCOUNTER4<-Mmupred
  Cchildcare10PREDCOUNTER4<-predchildcare10
  subsidizedprice
  
  #COMPUTE THE PRICE OF THE subsidy. Cinv12PREDCOUNTER4 gives the 
  #expenditure in investment. Of this, the household pays
  #1/TIMESUB and the government pays the remaining 1-1/Timesub 
  #fraction. Only for the beneficiaries
  EXPENDITURE4<-sum(EXPENDITURE4PC)
  EXPENDITURE4TOTAL[jjloop]<-EXPENDITURE4
  SKILLSCOUNTERFACTUAL4[1:950,jjloop]<-Cskills12PREDCOUNTER4
  }
  
  AMOUNTSPENTX4<-EXPENDITURE4TOTAL











#========================================#
#Organizing everything into one dataframe#
#========================================#

PREDFINAL<-data.frame(Mfraclabor12PRED,Ffraclabor12PRED,
                      data.Mage12,data.Fage12,
                      Mfraclabor10PRED,Ffraclabor10PRED,
                      data.Mfraclabor12,data.Ffraclabor12,
                      data.Mfraclabor10,data.Ffraclabor10,
                      data.Myrschool12,data.Fyrschool12,
                      data.Cliveswithfather12,data.Cliveswithmother12,
                      data.Cliveswithfather10,data.Cliveswithmother10,
                      data.Cchildcare10,predchildcare10,
                      data.Cedad_meses10,data.Mage10,data.Fage10,
                      Meffort12PREDCOUNTER1,
                      Feffort12PREDCOUNTER1,
                      Cinv12PREDCOUNTER1,
                      Cskills12PREDCOUNTER1,
                      Mfraclabor12PREDCOUNTER1,
                      Ffraclabor12PREDCOUNTER1,
                      Mfraclabor10PREDCOUNTER1,
                      Ffraclabor10PREDCOUNTER1,
                      Mmupred12PREDCOUNTER1,
                      Cchildcare10PREDCOUNTER1,
                      Meffort12PREDCOUNTER2,
                      Feffort12PREDCOUNTER2,
                      Cinv12PREDCOUNTER2,
                      Cskills12PREDCOUNTER2,
                      Mfraclabor12PREDCOUNTER2,
                      Ffraclabor12PREDCOUNTER2,
                      Mfraclabor10PREDCOUNTER2,
                      Ffraclabor10PREDCOUNTER2,
                      Mmupred12PREDCOUNTER2,
                      Cchildcare10PREDCOUNTER2,
                      Meffort12PREDCOUNTER3,
                      Feffort12PREDCOUNTER3,
                      Cinv12PREDCOUNTER3,
                      Cskills12PREDCOUNTER3,
                      Mfraclabor12PREDCOUNTER3,
                      Ffraclabor12PREDCOUNTER3,
                      Mfraclabor10PREDCOUNTER3,
                      Ffraclabor10PREDCOUNTER3,
                      Mmupred12PREDCOUNTER3,
                      Cchildcare10PREDCOUNTER3,
                      Meffort12PREDCOUNTER4,
                      Feffort12PREDCOUNTER4,
                      Cinv12PREDCOUNTER4,
                      Cskills12PREDCOUNTER4,
                      Mfraclabor12PREDCOUNTER4,
                      Ffraclabor12PREDCOUNTER4,
                      Mfraclabor10PREDCOUNTER4,
                      Ffraclabor10PREDCOUNTER4,
                      Mmupred12PREDCOUNTER4,
                      Cchildcare10PREDCOUNTER4,
                      Ffraclabor10PREDORIG,
                      Mfraclabor10PREDORIG,
                      Ffraclabor12PREDORIG,
                      Mfraclabor12PREDORIG,
                      Cinv12PREDORIG,
                      Cinv10PREDORIG,Meffort10ORIG,Meffort12ORIG,Feffort10ORIG,Feffort12ORIG)



#================#
#Generating plots#
#================#
#setwd('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/advances/reports/Figures')
setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/Counterfactuals/Graphs")




#===============================================================================#
#3.2 Distribution of skills according to income including the original situation#  
#===============================================================================#


#1. Ordering them by income does not actually show much


#Standardizing
Cskills12PREDORIGSTANDARD<-(log(Cskills12PREDORIG)-mean(log(Cskills12PREDORIG)))/sd(log(Cskills12PREDORIG))


#Computing for the first counterfactual the marginal returns:
EFFECT1<-numeric(LOOPLENGTH)
EFFECT2<-numeric(LOOPLENGTH)
EFFECT3<-numeric(LOOPLENGTH)
EFFECT4<-numeric(LOOPLENGTH)
#Standardized
SKILLLCOUNTERFACTUAL1STANDARD<-SKILLSCOUNTERFACTUAL1
SKILLLCOUNTERFACTUAL2STANDARD<-SKILLSCOUNTERFACTUAL2
SKILLLCOUNTERFACTUAL3STANDARD<-SKILLSCOUNTERFACTUAL3
SKILLLCOUNTERFACTUAL4STANDARD<-SKILLSCOUNTERFACTUAL4
#Standardized and organized by income
SKILLLCOUNTERFACTUAL1STANDARDORGANIZED<-SKILLSCOUNTERFACTUAL1
SKILLLCOUNTERFACTUAL2STANDARDORGANIZED<-SKILLSCOUNTERFACTUAL2
SKILLLCOUNTERFACTUAL3STANDARDORGANIZED<-SKILLSCOUNTERFACTUAL3
SKILLLCOUNTERFACTUAL4STANDARDORGANIZED<-SKILLSCOUNTERFACTUAL4
#Gap in 20-20 for first counterfactual
SKILLSGAP1<-numeric(LOOPLENGTH)
SKILLSGAP2<-numeric(LOOPLENGTH)
SKILLSGAP3<-numeric(LOOPLENGTH)
SKILLSGAP4<-numeric(LOOPLENGTH)

for (jj in 1:LOOPLENGTH){
  SKILLLCOUNTERFACTUAL1STANDARD[1:950,jj]<-(log(SKILLSCOUNTERFACTUAL1[1:950,jj])-mean(log(SKILLSCOUNTERFACTUAL1[1:950,jj])))/sd(log(SKILLSCOUNTERFACTUAL1[1:950,jj]))
  SKILLLCOUNTERFACTUAL2STANDARD[1:950,jj]<-(log(SKILLSCOUNTERFACTUAL2[1:950,jj])-mean(log(SKILLSCOUNTERFACTUAL2[1:950,jj])))/sd(log(SKILLSCOUNTERFACTUAL2[1:950,jj]))
  SKILLLCOUNTERFACTUAL3STANDARD[1:950,jj]<-(log(SKILLSCOUNTERFACTUAL3[1:950,jj])-mean(log(SKILLSCOUNTERFACTUAL3[1:950,jj])))/sd(log(SKILLSCOUNTERFACTUAL3[1:950,jj]))
  SKILLLCOUNTERFACTUAL4STANDARD[1:950,jj]<-(log(SKILLSCOUNTERFACTUAL4[1:950,jj])-mean(log(SKILLSCOUNTERFACTUAL4[1:950,jj])))/sd(log(SKILLSCOUNTERFACTUAL4[1:950,jj]))
}


#Now, organizing them:
Cskills12PREDORIGSTANDARDORDERED<-Cskills12PREDORIGSTANDARD[order(TOTINCOME)]
CSKILLS12ORIGAP<-mean(Cskills12PREDORIGSTANDARDORDERED[1:190])-mean(Cskills12PREDORIGSTANDARDORDERED[760:950])

for (jj in 1:LOOPLENGTH){
  SKILLLCOUNTERFACTUAL1STANDARDORGANIZED[1:950,jj]<-SKILLLCOUNTERFACTUAL1STANDARD[1:950,jj][order(TOTINCOME)]
  SKILLLCOUNTERFACTUAL2STANDARDORGANIZED[1:950,jj]<-SKILLLCOUNTERFACTUAL2STANDARD[1:950,jj][order(TOTINCOME)]
  SKILLLCOUNTERFACTUAL3STANDARDORGANIZED[1:950,jj]<-SKILLLCOUNTERFACTUAL3STANDARD[1:950,jj][order(TOTINCOME)]
  SKILLLCOUNTERFACTUAL4STANDARDORGANIZED[1:950,jj]<-SKILLLCOUNTERFACTUAL4STANDARD[1:950,jj][order(TOTINCOME)]
}



#And now the gap between 20 20 for counterfactuals
for(jj in 1:LOOPLENGTH){
  SKILLSGAP1[jj]<-mean(SKILLLCOUNTERFACTUAL1STANDARDORGANIZED[1:190,jj])-mean(SKILLLCOUNTERFACTUAL1STANDARDORGANIZED[760:950,jj])
  SKILLSGAP2[jj]<-mean(SKILLLCOUNTERFACTUAL2STANDARDORGANIZED[1:190,jj])-mean(SKILLLCOUNTERFACTUAL2STANDARDORGANIZED[760:950,jj])
  SKILLSGAP3[jj]<-mean(SKILLLCOUNTERFACTUAL3STANDARDORGANIZED[1:190,jj])-mean(SKILLLCOUNTERFACTUAL3STANDARDORGANIZED[760:950,jj])
  SKILLSGAP4[jj]<-mean(SKILLLCOUNTERFACTUAL4STANDARDORGANIZED[1:190,jj])-mean(SKILLLCOUNTERFACTUAL4STANDARDORGANIZED[760:950,jj])
}

#SKILLSGAP1REFOG<-100*(SKILLSGAP1-CSKILLS12ORIGAP)/CSKILLS12ORIGAP
#SKILLSGAP2REFOG<-100*(SKILLSGAP2-CSKILLS12ORIGAP)/CSKILLS12ORIGAP
#SKILLSGAP3REFOG<-100*(SKILLSGAP3-CSKILLS12ORIGAP)/CSKILLS12ORIGAP
#SKILLSGAP4REFOG<-100*(SKILLSGAP4-CSKILLS12ORIGAP)/CSKILLS12ORIGAP


SKILLSGAP1REFOG<-100*(SKILLSGAP1-CSKILLS12ORIGAP)
SKILLSGAP2REFOG<-100*(SKILLSGAP2-CSKILLS12ORIGAP)
SKILLSGAP3REFOG<-100*(SKILLSGAP3-CSKILLS12ORIGAP)
SKILLSGAP4REFOG<-100*(SKILLSGAP4-CSKILLS12ORIGAP)


#AND IF WE WANT THE AMOUNT SPENT TO BE PC:
AMOUNTSPENTX1PC=AMOUNTSPENTX1/190
AMOUNTSPENTX2PC=AMOUNTSPENTX2/190
AMOUNTSPENTX3PC=AMOUNTSPENTX3/190
AMOUNTSPENTX4PC=AMOUNTSPENTX4/190

#And to extend the line of the third subsidy, simply use AMOUNTSPENTX3PC=AMOUNTSPENTX4PC
AMOUNTSPENTX3PC=AMOUNTSPENTX1PC
#DOING THE GRAPHS:
countertable<-data.frame(AMOUNTSPENTX1,AMOUNTSPENTX2,AMOUNTSPENTX3,AMOUNTSPENTX4,
                         AMOUNTSPENTX1PC,AMOUNTSPENTX2PC,AMOUNTSPENTX3PC,AMOUNTSPENTX4PC,
                         SKILLSGAP1REFOG,SKILLSGAP2REFOG,SKILLSGAP3REFOG,SKILLSGAP4REFOG)


p<-ggplot(data=countertable)
p<-p+geom_line(aes(x=AMOUNTSPENTX1PC,y=SKILLSGAP1REFOG,color="Transfer to mother"),linetype=1,size=1.3)
p<-p+geom_line(aes(x=AMOUNTSPENTX2PC,y=SKILLSGAP2REFOG,color="Transfer to father"),linetype=2,size=1.3)
p<-p+geom_line(aes(x=AMOUNTSPENTX3PC,y=SKILLSGAP3REFOG,color="Childcare subsidy"),linetype=3,size=1.3)
p<-p+geom_line(aes(x=AMOUNTSPENTX4PC,y=SKILLSGAP4REFOG,color="Child investments subsidy"),linetype=4,size=1.3)
p<-p+scale_colour_manual(name = "", values = c("red","blue","green","brown"))
p<-p+scale_x_continuous(limits = c(0, 20000))
p<-p+scale_y_continuous(limits=c(0,7.5))
p<-p+labs(title="",x="Expenditure per household", y="Effect on skills")
p<-p + theme(
  plot.title = element_text(size = rel(2.0)),
  axis.title=element_text(size = rel(2.0)),
  axis.text.x=element_text(size = rel(2.0)),
  axis.text.y=element_text(size = rel(2.0)),
  legend.text=element_text(size = rel(1.5)),
  legend.position="bottom")
p<-p+guides(colour = guide_legend(nrow = 2))

pline<-p
dev.set()
pdf('COUNTERFACTUALSGRAPH.pdf')
pline
dev.off()








Cskills12PREDCOUNTER1STANDARD<-(log(Cskills12PREDCOUNTER1)-mean(log(Cskills12PREDCOUNTER1)))/sd(log(Cskills12PREDCOUNTER1))
Cskills12PREDCOUNTER2STANDARD<-(log(Cskills12PREDCOUNTER2)-mean(log(Cskills12PREDCOUNTER2)))/sd(log(Cskills12PREDCOUNTER2))
Cskills12PREDCOUNTER3STANDARD<-(log(Cskills12PREDCOUNTER3)-mean(log(Cskills12PREDCOUNTER3)))/sd(log(Cskills12PREDCOUNTER3))
Cskills12PREDCOUNTER4STANDARD<-(log(Cskills12PREDCOUNTER4)-mean(log(Cskills12PREDCOUNTER4)))/sd(log(Cskills12PREDCOUNTER4))

SKORIG<-Cskills12PREDORIGSTANDARD[order(TOTINCOME)]
SKP1<-Cskills12PREDCOUNTER1STANDARD[order(TOTINCOME)]
SKP2<-Cskills12PREDCOUNTER2STANDARD[order(TOTINCOME)]
SKP3<-Cskills12PREDCOUNTER3STANDARD[order(TOTINCOME)]
SKP4<-Cskills12PREDCOUNTER4STANDARD[order(TOTINCOME)]
XAXIS<-seq(1,950)


#Computing differences in mother's employment
ORIGSKILLS<-mean(SKORIG[1:190])-mean(SKORIG[760:950])
C1SKILLS<-mean(SKP1[1:190])-mean(SKP1[760:950])
C2SKILLS<-mean(SKP2[1:190])-mean(SKP2[760:950])
C3SKILLS<-mean(SKP3[1:190])-mean(SKP3[760:950])
C4SKILLS<-mean(SKP4[1:190])-mean(SKP4[760:950])

#Organizing the counterfactuals for a graph
SK<-c(ORIGSKILLS,C1SKILLS,C2SKILLS,C3SKILLS,C4SKILLS)
SK<--SK
SKSKILLS<-SK
SKResults<-data.frame(SK,Legend)



#####
#6. Skils
####

CFSK1<-100*-(SKSKILLS[2]-SKSKILLS[1])
CFSK2<-100*-(SKSKILLS[3]-SKSKILLS[1])
CFSK3<-100*-(SKSKILLS[4]-SKSKILLS[1])
CFSK4<-100*-(SKSKILLS[5]-SKSKILLS[1])



CFSK<-rbind(CFSK1,CFSK2,CFSK3,CFSK4)








##Total counterfactuals
TCF<-cbind(CFSK)

rownames(TCF)<-c("Cash transfer to mother",
                 "Cash transfer to father",
                 "Childcare subsidy",
                 "Child-investments subsidy")

colnames(TCF)<-c("Skills of children")



#Note to be inserted
#Footnote to be inserted
comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(TCF))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            "\\multicolumn{2}{l}{For each question parents reply how often, during the last seven days, }\\\\ \n",
                            "\\multicolumn{2}{l}{they perform each activity. The possible answers are: Never, 1-3 times, 4-6 times.} \n",
                            sep = ""))

DT<-2


#tabout<-xtable(TCF,caption="Measures used for Pareto weight",digits=DT,label="tab:TableMeasuresBarg",table.placement="H")
#align(tabout)<-"lc"
#write(print(tabout, table.placement="H", sanitize.text.function=function(x){x},include.rownames=(TRUE),size="\\footnotesize",add.to.row = comment,hline.after = c(-1, 0)),file="TablePolicychange.tex")



