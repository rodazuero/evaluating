
rm(list=ls(all=TRUE))

#Need the package "numDeriv

#Run the necessary functions and libraries
library(ggplot2)
library(gridExtra)
library(numDeriv)
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

#0.2Directory where the final estimates are going to be stored (the tables)
tabledir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL71/ParameterEstimates"


#1. Thickness of lines
sizegraphs=2 

#--------------------------#
#Running the rcpp functions#
#--------------------------#

#Not necessary for the signal to noise ratio
Rcpp::sourceCpp('/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/maincppStandarderrors.cpp')
algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/PAR32.csv", sep=",", header=FALSE)

paropt<-algo[,1]



#------------------------------------------------------------#
#-Definition of parameters with the optimal parameters found-#
#------------------------------------------------------------#

aalpha1m=exp(paropt[1])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
aalpha2m=exp(paropt[2])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
aalpha3m=exp(paropt[3])/(exp(paropt[1])+exp(paropt[2])+exp(paropt[3])+exp(paropt[4]))
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
price=exp(paropt[20])
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

aalpha3m10=exp(paropt[50])/
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

aalpha3m10_mtjh=(exp(paropt[74])/(1+exp(paropt[74])))*aalpha3m10
aalpha3m12_mtjh=(exp(paropt[75])/(1+exp(paropt[75])))*aalpha3m

aalpha3mage10=(exp(paropt[76])/(1+exp(paropt[76])))*aalpha3m10
aalpha3mage12=(exp(paropt[77])/(1+exp(paropt[77])))*aalpha3m
ddelta4=paropt[78]
#Skills in 2010
MS1_10=paropt[79]
VS1_10=exp(paropt[80])
MS2_10=paropt[81]
VS2_10=exp(paropt[82])
MS3_10=paropt[83]
VS3_10=exp(paropt[84])
MS4_10=paropt[85]
VS4_10=exp(paropt[86])
MS5_10=paropt[87]
VS5_10=exp(paropt[88])
MS6_10=paropt[89]
VS6_10=exp(paropt[90])
MS7_10=paropt[91]
VS7_10=exp(paropt[92])
MS8_10=paropt[93]
VS8_10=exp(paropt[94])
MS9_10=paropt[95]
VS9_10=exp(paropt[96])
MS10_10=paropt[97]
VS10_10=exp(paropt[98])
MS11_10=paropt[99]
VS11_10=exp(paropt[100])
#skills in 2012
MS1_12=paropt[101]
VS1_12=exp(paropt[102])
MS2_12=paropt[103]
VS2_12=exp(paropt[104])
MS3_12=paropt[105]
VS3_12=exp(paropt[106])
MS4_12=paropt[107]
VS4_12=exp(paropt[108])
MS5_12=paropt[109]
VS5_12=exp(paropt[110])
MS6_12=paropt[111]
VS6_12=exp(paropt[112])
MS7_12=paropt[113]
VS7_12=exp(paropt[114])
MS8_12=paropt[115]
VS8_12=exp(paropt[116])
MS9_12=paropt[117]
VS9_12=exp(paropt[118])
MS10_12=paropt[119]
VS10_12=exp(paropt[120])
MS11_12=paropt[121]
VS11_12=exp(paropt[122])
MS12_12=paropt[123]
VS12_12=exp(paropt[124])
MS13_12=paropt[125]
VS13_12=exp(paropt[126])
#Conditions during pregnancy skills at birth
MS1_BIRTH=paropt[127]
VS1_BIRTH=exp(paropt[128])
MS2_BIRTH=paropt[129]
VS2_BIRTH=exp(paropt[130])
MS3_BIRTH=paropt[131]
VS3_BIRTH=exp(paropt[132])
MS4_BIRTH=paropt[133]
VS4_BIRTH=exp(paropt[134])
MS5_BIRTH=paropt[135]
VS5_BIRTH=exp(paropt[136])
MS6_BIRTH=paropt[137]
VS6_BIRTH=exp(paropt[138])
MS7_BIRTH=paropt[139]
VS7_BIRTH=exp(paropt[140])
MS8_BIRTH=paropt[141]
VS8_BIRTH=exp(paropt[142])
MS9_BIRTH=paropt[143]
VS9_BIRTH=exp(paropt[144])
MS10_BIRTH=paropt[145]
VS10_BIRTH=exp(paropt[146])
MS11_BIRTH=paropt[147]
VS11_BIRTH=exp(paropt[148])
MS12_BIRTH=paropt[149]
VS12_BIRTH=exp(paropt[150])
MS13_BIRTH=paropt[151]
VS13_BIRTH=exp(paropt[152])
MS14_BIRTH=paropt[153]
VS14_BIRTH=exp(paropt[154])
MS15_BIRTH=paropt[155]
VS15_BIRTH=exp(paropt[156])
MS16_BIRTH=paropt[157]
VS16_BIRTH=exp(paropt[158])
MS17_BIRTH=paropt[159]
VS17_BIRTH=exp(paropt[160])
MS18_BIRTH=paropt[161]
VS18_BIRTH=exp(paropt[162])
MS19_BIRTH=paropt[163]
VS19_BIRTH=exp(paropt[164])
MS20_BIRTH=paropt[165]
VS20_BIRTH=exp(paropt[166])
MS21_BIRTH=paropt[167]
VS21_BIRTH=exp(paropt[168])
MS22_BIRTH=paropt[169]
VS22_BIRTH=exp(paropt[170])
MS23_BIRTH=paropt[171]
VS23_BIRTH=exp(paropt[172])

#Skills of primary caregiver
MS1_PG=paropt[173]
VS1_PG=exp(paropt[174])
MS2_PG=paropt[175]
VS2_PG=exp(paropt[176])
MS3_PG=paropt[177]
VS3_PG=exp(paropt[178])
MS4_PG=paropt[179]
VS4_PG=exp(paropt[180])
MS5_PG=paropt[181]
VS5_PG=exp(paropt[182])
MS6_PG=paropt[183]
VS6_PG=exp(paropt[184])
MS7_PG=paropt[185]
VS7_PG=exp(paropt[186])
MS8_PG=paropt[187]
VS8_PG=exp(paropt[188])
#BARGAINING POWER
MS1_BARG=paropt[189]
VS1_BARG=exp(paropt[190])
MS2_BARG=paropt[191]
VS2_BARG=exp(paropt[192])
MS3_BARG=paropt[193]
VS3_BARG=exp(paropt[194])
MS4_BARG=paropt[195]
VS4_BARG=exp(paropt[196])
MS5_BARG=paropt[197]
VS5_BARG=exp(paropt[198])
MS6_BARG=paropt[199]
VS6_BARG=exp(paropt[200])
MS7_BARG=paropt[201]
VS7_BARG=exp(paropt[202])
MS8_BARG=paropt[203]
VS8_BARG=exp(paropt[204])
MS9_BARG=paropt[205]
VS9_BARG=exp(paropt[206])
MS10_BARG=paropt[207]
VS10_BARG=exp(paropt[208])
MS11_BARG=paropt[209]
VS11_BARG=exp(paropt[210])
MS12_BARG=paropt[211]
VS12_BARG=exp(paropt[212])
MS13_BARG=paropt[213]
VS13_BARG=exp(paropt[214])
MS14_BARG=paropt[215]
VS14_BARG=exp(paropt[216])
MS15_BARG=paropt[217]
VS15_BARG=exp(paropt[218])
MS16_BARG=paropt[219]
VS16_BARG=exp(paropt[220])
MS17_BARG=paropt[221]
VS17_BARG=exp(paropt[222])
MS18_BARG=paropt[223]
VS18_BARG=exp(paropt[224])
MS19_BARG=paropt[225]
VS19_BARG=exp(paropt[226])
#INVESTMENTS IN 2010

MS1_INV10=paropt[227]
VS1_INV10=exp(paropt[228])
MS2_INV10=paropt[229]
VS2_INV10=exp(paropt[230])
MS3_INV10=paropt[231]
VS3_INV10=exp(paropt[232])
MS4_INV10=paropt[233]
VS4_INV10=exp(paropt[234])
MS5_INV10=paropt[235]
VS5_INV10=exp(paropt[236])
MS6_INV10=paropt[237]
VS6_INV10=exp(paropt[238])
MS7_INV10=paropt[239]
VS7_INV10=exp(paropt[240])
MS8_INV10=paropt[241]
VS8_INV10=exp(paropt[242])


#INVESTMENTS IN 2012
MS1_INV12=paropt[243]
VS1_INV12=exp(paropt[244])
MS2_INV12=paropt[245]
VS2_INV12=exp(paropt[246])
MS3_INV12=paropt[247]
VS3_INV12=exp(paropt[248])
MS4_INV12=paropt[249]
VS4_INV12=exp(paropt[250])
MS5_INV12=paropt[251]
VS5_INV12=exp(paropt[252])
MS6_INV12=paropt[253]
VS6_INV12=exp(paropt[254])
MS7_INV12=paropt[255]
VS7_INV12=exp(paropt[256])
MS8_INV12=paropt[257]
VS8_INV12=exp(paropt[258])
MS9_INV12=paropt[259]
VS9_INV12=exp(paropt[260])
MS10_INV12=paropt[261]
VS10_INV12=exp(paropt[262])
MS11_INV12=paropt[263]
VS11_INV12=exp(paropt[264])
MS12_INV12=paropt[265]
VS12_INV12=exp(paropt[266])
MS13_INV12=paropt[267]
VS13_INV12=exp(paropt[268])
MS14_INV12=paropt[269]
VS14_INV12=exp(paropt[270])
MS15_INV12=paropt[271]
VS15_INV12=exp(paropt[272])
MS16_INV12=paropt[273]
VS16_INV12=exp(paropt[274])
MS17_INV12=paropt[275]
VS17_INV12=exp(paropt[276])
MS18_INV12=paropt[277]
VS18_INV12=exp(paropt[278])
MS19_INV12=paropt[279]
VS19_INV12=exp(paropt[280])
MS20_INV12=paropt[281]
VS20_INV12=exp(paropt[282])
MS21_INV12=paropt[283]
VS21_INV12=exp(paropt[284])

#EFFORT IN 2010
MS1_EF10=paropt[285]
VS1_EF10=exp(paropt[286])
MS2_EF10=paropt[287]
VS2_EF10=exp(paropt[288])
MS3_EF10=paropt[289]
VS3_EF10=exp(paropt[290])
MS4_EF10=paropt[291]
VS4_EF10=exp(paropt[292])
MS5_EF10=paropt[293]
VS5_EF10=exp(paropt[294])
MS6_EF10=paropt[295]
VS6_EF10=exp(paropt[296])


#EFFORT IN 2012
MS1_EF12=paropt[297]
VS1_EF12=exp(paropt[298])
MS2_EF12=paropt[299]
VS2_EF12=exp(paropt[300])
MS3_EF12=paropt[301]
VS3_EF12=exp(paropt[302])
MS4_EF12=paropt[303]
VS4_EF12=exp(paropt[304])
MS5_EF12=paropt[305]
VS5_EF12=exp(paropt[306])
MS6_EF12=paropt[307]
VS6_EF12=exp(paropt[308])
MS7_EF12=paropt[309]
VS7_EF12=exp(paropt[310])
MS8_EF12=paropt[311]
VS8_EF12=exp(paropt[312])
MS9_EF12=paropt[313]
VS9_EF12=exp(paropt[314])
MS10_EF12=paropt[315]
VS10_EF12=exp(paropt[316])
MS11_EF12=paropt[317]
VS11_EF12=exp(paropt[318])
MS12_EF12=paropt[319]
VS12_EF12=exp(paropt[320])
MS13_EF12=paropt[321]
VS13_EF12=exp(paropt[322])
MS14_EF12=paropt[323]
VS14_EF12=exp(paropt[324])
VPRICE1=exp(paropt[325])


par=c(aalpha1m,aalpha2m,aalpha3m,aalpha40m,aalpha41m,
      ttheta0,ttheta1,ttheta2,pphi,stdskills,
      ggammaf,ggammam,
      bbeta0m,bbeta1m,bbeta2m,bbeta3m,stdwm,stdeffort,stdinv,
      price,ddelta0,ddelta1,ddelta2,
      ddelta3,
      ddelta3_12,
      aalpha1f,
      aalpha2f,
      aalpha3f,
      aalpha40f,
      aalpha41f,
      bbeta0f,
      bbeta1f,
      bbeta2f,
      bbeta3f,
      stdwf,
      stdeffat,
      llambda0,
      llambda1,
      llambda2,
      llambda3,
      llambda4,
      stdmmu,
      aalpha1m10,
      aalpha2m10,
      aalpha3m10,
      aalpha40m10,
      aalpha5m10,aalpha41m10,aalpha1f10,aalpha2f10,aalpha3f,
      aalpha40f10,
      aalpha5f10,aalpha41f10,
      
      pchildcare0,
      pchildcare1,
      MshockWA,
      MshockNWA,
      MshockWNA,
      MshockNWNA,
      FshockWA,
      FshockNWA,
      FshockWNA,
      FshockNWNA,      
      llambda5,
      llambda6,
      llambda7,
      llambda8,
      ddelta4,
      
      
      MS1_10,
      VS1_10,
      MS2_10,
      VS2_10,
      MS3_10,
      VS3_10,
      
      MS5_10,
      VS5_10,
      MS6_10,
      VS6_10,
      MS7_10,
      VS7_10,
      MS8_10,
      VS8_10,
      MS9_10,
      VS9_10,
      MS10_10,
      VS10_10,
      MS11_10,
      VS11_10,
      
      MS1_12,
      VS1_12,
      MS2_12,
      VS2_12,
      MS3_12,
      VS3_12,
      MS4_12,
      VS4_12,
      MS5_12,
      VS5_12,
      MS6_12,
      VS6_12,
      MS7_12,
      VS7_12,
      MS8_12,
      VS8_12,
      MS9_12,
      VS9_12,
      MS10_12,
      VS10_12,
      
      MS13_12,
      VS13_12,
      
      MS1_BIRTH,
      VS1_BIRTH,
      MS2_BIRTH,
      VS2_BIRTH,
      MS3_BIRTH,
      VS3_BIRTH,
      MS4_BIRTH,
      VS4_BIRTH,
      MS5_BIRTH,
      VS5_BIRTH,
      MS6_BIRTH,
      VS6_BIRTH,
      MS7_BIRTH,
      VS7_BIRTH,
      MS8_BIRTH,
      VS8_BIRTH,
      MS9_BIRTH,
      VS9_BIRTH,
      MS10_BIRTH,
      VS10_BIRTH,
      MS11_BIRTH,
      VS11_BIRTH,
      MS12_BIRTH,
      VS12_BIRTH,
      MS13_BIRTH,
      VS13_BIRTH,
      MS14_BIRTH,
      VS14_BIRTH,
      MS15_BIRTH,
      VS15_BIRTH,
      MS16_BIRTH,
      VS16_BIRTH,
      MS17_BIRTH,
      VS17_BIRTH,
      MS18_BIRTH,
      VS18_BIRTH,
      MS19_BIRTH,
      VS19_BIRTH,
      MS20_BIRTH,
      VS20_BIRTH,
      MS21_BIRTH,
      VS21_BIRTH,
      MS22_BIRTH,
      VS22_BIRTH,
      MS23_BIRTH,
      VS23_BIRTH,
      
      MS1_PG,
      VS1_PG,
      
      MS2_PG,
      VS2_PG,
      
      MS3_PG,
      VS3_PG,
      
      MS4_PG,
      VS4_PG,
      MS5_PG,
      VS5_PG,
      
      
      
      MS6_PG,
      VS6_PG,
      
      MS7_PG,
      VS7_PG,
      MS8_PG,
      VS8_PG,
      
      MS1_BARG,
      VS1_BARG,
      MS2_BARG,
      VS2_BARG,
      MS3_BARG,
      VS3_BARG,
      MS4_BARG,
      VS4_BARG,
      MS5_BARG,
      VS5_BARG,
      MS6_BARG,
      VS6_BARG,
      MS7_BARG,
      VS7_BARG,
      MS8_BARG,
      VS8_BARG,
      MS9_BARG,
      VS9_BARG,
      MS10_BARG,
      VS10_BARG,
      MS11_BARG,
      VS11_BARG,
      MS12_BARG,
      VS12_BARG,
      MS13_BARG,
      VS13_BARG,
      MS14_BARG,
      VS14_BARG,
      MS15_BARG,
      VS15_BARG,
      MS16_BARG,
      VS16_BARG,
      MS17_BARG,
      VS17_BARG,
      MS18_BARG,
      VS18_BARG,
      MS19_BARG,
      VS19_BARG,
      
      MS1_INV10,
      VS1_INV10,
      MS2_INV10,
      VS2_INV10,
      MS3_INV10,
      VS3_INV10,
      MS4_INV10,
      VS4_INV10,
      MS5_INV10,
      VS5_INV10,
      MS6_INV10,
      VS6_INV10,
      MS7_INV10,
      VS7_INV10,
      MS8_INV10,
      VS8_INV10,
      
      MS1_INV12,
      VS1_INV12,
      MS2_INV12,
      VS2_INV12,
      MS3_INV12,
      VS3_INV12,
      MS4_INV12,
      VS4_INV12,
      MS5_INV12,
      VS5_INV12,
      MS6_INV12,
      VS6_INV12,
      MS7_INV12,
      VS7_INV12,
      MS8_INV12,
      VS8_INV12,
      MS9_INV12,
      VS9_INV12,
      MS10_INV12,
      VS10_INV12,
      MS11_INV12,
      VS11_INV12,
      MS12_INV12,
      VS12_INV12,
      MS13_INV12,
      VS13_INV12,
      MS14_INV12,
      VS14_INV12,
      MS15_INV12,
      VS15_INV12,
      MS16_INV12,
      VS16_INV12,
      MS17_INV12,
      VS17_INV12,
      MS18_INV12,
      VS18_INV12,
      MS19_INV12,
      VS19_INV12,
      MS20_INV12,
      VS20_INV12,
      MS21_INV12,
      VS21_INV12,
      
      MS1_EF10,
      VS1_EF10,
      MS2_EF10,
      VS2_EF10,
      MS3_EF10,
      VS3_EF10,
      MS4_EF10,
      VS4_EF10,
      MS5_EF10,
      VS5_EF10,
      MS6_EF10,
      VS6_EF10,
      MS1_EF12,
      VS1_EF12,
      MS2_EF12,
      VS2_EF12,
      MS3_EF12,
      VS3_EF12,
      MS4_EF12,
      VS4_EF12,
      MS5_EF12,
      VS5_EF12,
      MS6_EF12,
      VS6_EF12,
      MS7_EF12,
      VS7_EF12,
      MS8_EF12,
      VS8_EF12,
      MS9_EF12,
      VS9_EF12,
      MS10_EF12,
      VS10_EF12,
      MS11_EF12,
      VS11_EF12,
      MS12_EF12,
      VS12_EF12,
      MS13_EF12,
      VS13_EF12,
      MS14_EF12,
      VS14_EF12,
      VPRICE1)

#Test the function
F_likelihood_FIN(par)

parTITLES=c("$\\alpha_{1,2}^m$",
            "$\\alpha_{2,2}^m$",
            "$\\alpha_{3,2}^m$",
            "$\\alpha_{4,0,2}^m$",
            "$\\alpha_{4,1,2}^m$",
        "$\\theta_0$",
        "$\\theta_1$",
        "$\\theta_2$",
        "$\\phi$",
        "$\\sigma_{s}$",
        "$\\gamma_f$",
        "$\\gamma_m$",
        "$\\beta_0^m$",
        "$\\beta_1^m$",
        "$\\beta_2^m$",
        "$\\beta_3^m$",
        "$\\sigma_{w_m}$",
        "$\\sigma_{ef}^m$",
        "$\\sigma_{inv}$",
        "Price$_{I_0}$","$\\delta_0$","$\\delta_1$","$\\delta_2$",
        "$\\delta_{3,1}$",
        "$\\delta_{3,2}$",
        "$\\alpha_{1,2}^f$",
        "$\\alpha_{2,2}^f$",
        "$\\alpha_{3,2}^f$",
        "$\\alpha_{4,0,2}^f$",
        "$\\alpha_{4,1,2}^f$",
        "$\\beta_0^f$",
        "$\\beta_1^f$",
        "$\\beta_2^f$",
        "$\\beta_3^f$",
        "$\\sigma_{w_f}$",
        "$\\sigma_{ef}^f$",
        "$\\lambda_0$",
        "$\\lambda_1$",
        "$\\lambda_2$",
        "$\\lambda_3$",
        "$\\lambda_4$",
        "$\\sigma_{\\mu}$",
        "$MEASSkills$",
        "$MEASFeffort$",
        "$MEASMeffort$",
        "$MEASMMu$",
        "$MEASINV$",
        "$\\alpha_{1,1}^m$",
        "$\\alpha_{2,1}^m$",
        "$\\alpha_{3,1}^m$",
        "$\\alpha_{4,0,1}^m$",
        "$\\alpha_{6,1}^m$",
        "$\\alpha_{4,1,1}^{m}$",
        "$\\alpha_{1,1}^f$","$\\alpha_{2,1}^f$","$\\alpha_{3,1}^f$",
        "$\\alpha_{4,0,1}^f$",
        "$\\alpha_{6,10}^f$","$\\alpha_{4,1,1}^{f}$","Pchildcare$_0$",
        "Pchildcare$_1$",
        "$\\sigma_{W,A}^m$",
        "$\\sigma_{NW,A}^m$",
        "$\\sigma_{W,NA}^m$",
        "$\\sigma_{NW,NA}^m$",
        "$\\sigma_{W,A}^f$",
        "$\\sigma_{NW,A}^f$",
        "$\\sigma_{W,NA}^f$",
        "$\\sigma_{NW,NA}^f$",
        "$\\lambda_5$",
        "$\\lambda_6$",
        "$\\lambda_7$",
        "$\\lambda_8$",
        "$\\alpha_{3,1,mtjh}$",
        "$\\alpha_{3,2,mtjh}$",
        "$\\alpha_{3,A,1}^m$",
        "$\\alpha_{3,A,2}^m$",
        "$\\delta_4$",
        "MS$_{1,1}$",
        "SDS$_{1,1}$",
        "MS$_{2,1}$",
        "SDS$_{2,1}$",
        "MS$_{3,1}$",
        "SDS$_{3,1}$",
        "MS$_{4,1}$",
        "SDS$_{4,1}$",
        "MS$_{4,1}$",
        "SDS$_{4,1}$",
        "MS$_{5,1}$",
        "SDS$_{5,1}$",
        "MS$_{6,1}$",
        "SDS$_{6,1}$",
        "MS$_{7,1}$",
        "SDS$_{7,1}$",
        "MS$_{8,1}$",
        "SDS$_{8,1}$",
        "MS$_{9_1}$",
        "SDS$_{9_1}$",
        "MS$_{10_1}$",
        "SDS$_{10_1}$",
        "MS$_{1_2}$",
        "SDS$_{1_2}$",
        "MS$_{2_2}$",
        "SDS$_{2_2}$",
        "MS$_{3_2}$",
        "SDS$_{3_2}$",
        "MS$_{4_2}$",
        "SDS$_{4_2}$",
        "MS$_{5_2}$",
        "SDS$_{5_2}$",
        "MS$_{6_2}$",
        "SDS$_{6_2}$",
        "MS$_{7_2}$",
        "SDS$_{7_2}$",
        "MS$_{8_2}$",
        "SDS$_{8_2}$",
        "MS$_{9_2}$",
        "SDS$_{9_2}$",
        "MS$_{10_2}$",
        "SDS$_{10_2}$",
        "MS$_{11_2}$",
        "SDS$_{11_2}$",
        "MS$_{12_2}$",
        "SDS$_{12_2}$",
        "MS$_{11_2}$",
        "SDS$_{11_2}$",
        "MS$_{1_{BIRTH}}$",
        "SDS$_{1_{BIRTH}}$",
        "MS$_{2_{BIRTH}}$",
        "SDS$_{2_{BIRTH}}$",
        "MS$_{3_{BIRTH}}$",
        "SDS$_{3_{BIRTH}}$",
        "MS$_{4_{BIRTH}}$",
        "SDS$_{4_{BIRTH}}$",
        "MS$_{5_{BIRTH}}$",
        "SDS$_{5_{BIRTH}}$",
        "MS$_{6_{BIRTH}}$",
        "SDS$_{6_{BIRTH}}$",
        "MS$_{7_{BIRTH}}$",
        "SDS$_{7_{BIRTH}}$",
        "MS$_{8_{BIRTH}}$",
        "SDS$_{8_{BIRTH}}$",
        "MS$_{9_{BIRTH}}$",
        "SDS$_{9_{BIRTH}}$",
        "MS$_{10_{BIRTH}}$",
        "SDS$_{10_{BIRTH}}$",
        "MS$_{11_{BIRTH}}$",
        "SDS$_{11_{BIRTH}}$",
        "MS$_{12_{BIRTH}}$",
        "SDS$_{12_{BIRTH}}$",
        "MS$_{13_{BIRTH}}$",
        "SDS$_{13_{BIRTH}}$",
        "MS$_{14_{BIRTH}}$",
        "SDS$_{14_{BIRTH}}$",
        "MS$_{15_{BIRTH}}$",
        "SDS$_{15_{BIRTH}}$",
        "MS$_{16_{BIRTH}}$",
        "SDS$_{16_{BIRTH}}$",
        "MS$_{17_{BIRTH}}$",
        "SDS$_{17_{BIRTH}}$",
        "MS$_{18_{BIRTH}}$",
        "SDS$_{18_{BIRTH}}$",
        "MS$_{19_{BIRTH}}$",
        "SDS$_{19_{BIRTH}}$",
        "MS$_{20_{BIRTH}}$",
        "SDS$_{20_{BIRTH}}$",
        "MS$_{21_{BIRTH}}$",
        "SDS$_{21_{BIRTH}}$",
        "MS$_{22_{BIRTH}}$",
        "SDS$_{22_{BIRTH}}$",
        "MS$_{23_{BIRTH}}$",
        "SDS$_{23_{BIRTH}}$",
        "MS$_{1,PG}$",
        "SDS$_{1,PG}$",
        "MS$_{2,PG}$",
        "SDS$_{2,PG}$",
        "MS$_{3,PG}$",
        "SDS$_{3,PG}$",
        "MS$_{4,PG}$",
        "SDS$_{4,PG}$",
        "MS$_{5,PG}$",
        "SDS$_{5,PG}$",
        "MS$_{6,PG}$",
        "SDS$_{6,PG}$",
        "MS$_{7,PG}$",
        "SDS$_{7,PG}$",
        "MS$_{8,PG}$",
        "SDS$_{8,PG}$",
        "MS$_{1_{BARG}}$",
        "SDS$_{1_{BARG}}$",
        "MS$_{2_{BARG}}$",
        "SDS$_{2_{BARG}}$",
        "MS$_{3_{BARG}}$",
        "SDS$_{3_{BARG}}$",
        "MS$_{4_{BARG}}$",
        "SDS$_{4_{BARG}}$",
        "MS$_{5_{BARG}}$",
        "SDS$_{5_{BARG}}$",
        "MS$_{6_{BARG}}$",
        "SDS$_{6_{BARG}}$",
        "MS$_{7_{BARG}}$",
        "SDS$_{7_{BARG}}$",
        "MS$_{8_{BARG}}$",
        "SDS$_{8_{BARG}}$",
        "MS$_{9_{BARG}}$",
        "SDS$_{9_{BARG}}$",
        "MS$_{10_{BARG}}$",
        "SDS$_{10_{BARG}}$",
        "MS$_{11_{BARG}}$",
        "SDS$_{11_{BARG}}$",
        "MS$_{12_{BARG}}$",
        "SDS$_{12_{BARG}}$",
        "MS$_{13_{BARG}}$",
        "SDS$_{13_{BARG}}$",
        "MS$_{14_{BARG}}$",
        "SDS$_{14_{BARG}}$",
        "MS$_{1N_{BARG}}$",
        "SDS$_{1N_{BARG}}$",
        "MS$_{15_{BARG}}$",
        "SDS$_{15_{BARG}}$",
        "MS$_{16_{BARG}}$",
        "SDS$_{16_{BARG}}$",
        "MS$_{17_{BARG}}$",
        "SDS$_{17_{BARG}}$",
        "MS$_{18_{BARG}}$",
        "SDS$_{18_{BARG}}$",
        "MS$_{1_{INV,1}}$",
        "SDS$_{1_{INV,1}}$",
        "MS$_{2_{INV,1}}$",
        "SDS$_{2_{INV,1}}$",
        "MS$_{3_{INV,1}}$",
        "SDS$_{3_{INV,1}}$",
        "MS$_{4_{INV,1}}$",
        "SDS$_{4_{INV,1}}$",
        "MS$_{5_{INV,1}}$",
        "SDS$_{5_{INV,1}}$",
        "MS$_{6_{INV,1}}$",
        "SDS$_{6_{INV,1}}$",
        "MS$_{7_{INV,1}}$",
        "SDS$_{7_{INV,1}}$",
        "MS$_{8_{INV,1}}$",
        "SDS$_{8_{INV,1}}$",
        "MS$_{1_{INV,2}}$",
        "SDS$_{1_{INV,2}}$",
        "MS$_{2_{INV,2}}$",
        "SDS$_{2_{INV,2}}$",
        "MS$_{3_{INV,2}}$",
        "SDS$_{3_{INV,2}}$",
        "MS$_{4_{INV,2}}$",
        "SDS$_{4_{INV,2}}$",
        "MS$_{5_{INV,2}}$",
        "SDS$_{5_{INV,2}}$",
        "MS$_{6_{INV,2}}$",
        "SDS$_{6_{INV,2}}$",
        "MS$_{7_{INV,2}}$",
        "SDS$_{7_{INV,2}}$",
        "MS$_{8_{INV,2}}$",
        "SDS$_{8_{INV,2}}$",
        "MS$_{9_{INV,2}}$",
        "SDS$_{9_{INV,2}}$",
        "MS$_{10_{INV,2}}$",
        "SDS$_{10_{INV,2}}$",
        "MS$_{11_{INV,2}}$",
        "SDS$_{11_{INV,2}}$",
        "MS$_{12_{INV,2}}$",
        "SDS$_{12_{INV,2}}$",
        "MS$_{13_{INV,2}}$",
        "SDS$_{13_{INV,2}}$",
        "MS$_{14_{INV,2}}$",
        "SDS$_{14_{INV,2}}$",
        "MS$_{15_{INV,2}}$",
        "SDS$_{15_{INV,2}}$",
        "MS$_{16_{INV,2}}$",
        "SDS$_{16_{INV,2}}$",
        "MS$_{17_{INV,2}}$",
        "SDS$_{17_{INV,2}}$",
        "MS$_{18_{INV,2}}$",
        "SDS$_{18_{INV,2}}$",
        "MS$_{19_{INV,2}}$",
        "SDS$_{19_{INV,2}}$",
        "MS$_{20_{INV,2}}$",
        "SDS$_{20_{INV,2}}$",
        "MS$_{21_{INV,2}}$",
        "SDS$_{21_{INV,2}}$",
        "MS$_{1_{EF,1}}$",
        "SDS$_{1_{EF,1}}$",
        "MS$_{2_{EF,1}}$",
        "SDS$_{2_{EF,1}}$",
        "MS$_{3_{EF,1}}$",
        "SDS$_{3_{EF,1}}$",
        "MS$_{4_{EF,1}}$",
        "SDS$_{4_{EF,1}}$",
        "MS$_{5_{EF,1}}$",
        "SDS$_{5_{EF,1}}$",
        "MS$_{6_{EF,1}}$",
        "SDS$_{6_{EF,1}}$",
        "MS$_{1_{EF,2}}$",
        "SDS$_{1_{EF,2}}$",
        "MS$_{2_{EF,2}}$",
        "SDS$_{2_{EF,2}}$",
        "MS$_{3_{EF,2}}$",
        "SDS$_{3_{EF,2}}$",
        "MS$_{4_{EF,2}}$",
        "SDS$_{4_{EF,2}}$",
        "MS$_{5_{EF,2}}$",
        "SDS$_{5_{EF,2}}$",
        "MS$_{6_{EF,2}}$",
        "SDS$_{6_{EF,2}}$",
        "MS$_{7_{EF,2}}$",
        "SDS$_{7_{EF,2}}$",
        "MS$_{8_{EF,2}}$",
        "SDS$_{8_{EF,2}}$",
        "MS$_{9_{EF,2}}$",
        "SDS$_{9_{EF,2}}$",
        "MS$_{10_{EF12}}$",
        "SDS$_{10_{EF,2}}$",
        "MS$_{11_{EF,2}}$",
        "SDS$_{11_{EF,2}}$",
        "MS$_{12_{EF,2}}$",
        "SDS$_{12_{EF,2}}$",
        "MS$_{13_{EF,2}}$",
        "SDS$_{13_{EF,2}}$",
        "MS$_{14_{EF,2}}$",
        "SDS$_{14_{EF,2}}$",
        "Price$_{I_1}$")



#OPT=optim(par,F_likelihood_FIN,  method = "BFGS", hessian = TRUE,control=list(maxit=1))

#F_likelihood_FIN(par)

#hess <- hessian(func=F_likelihood_FIN, x=par)


VARCOV=seq(1,325)
VARCOV<-0*VARCOV
partest=c(aalpha1m,aalpha2m,aalpha3m,aalpha40m,aalpha41m,
          ttheta0,ttheta1,ttheta2,pphi,stdskills,
          ggammaf,ggammam,
          bbeta0m,bbeta1m,bbeta2m,bbeta3m,stdwm,stdeffort,stdinv,
          price,ddelta0,ddelta1,ddelta2,
          ddelta3,
          ddelta3_12,
          aalpha1f,
          aalpha2f,
          aalpha3f,
          aalpha40f,
          aalpha41f,
          bbeta0f,
          bbeta1f,
          bbeta2f,
          bbeta3f,
          stdwf,
          stdeffat,
          llambda0,
          llambda1,
          llambda2,
          llambda3,
          llambda4,
          stdmmu,
          MEASSkills,
          MEASFeffort,
          MEASMeffort,
          MEASMMu,
          MEASINV,
          aalpha1m10,
          aalpha2m10,
          aalpha3m10,
          aalpha40m10,
          aalpha5m10,aalpha41m10,aalpha1f10,aalpha2f10,aalpha3f,
          aalpha40f10,
          aalpha5f10,aalpha41f10,pchildcare0,
          pchildcare1,
          MshockWA,
          MshockNWA,
          MshockWNA,
          MshockNWNA,
          FshockWA,
          FshockNWA,
          FshockWNA,
          FshockNWNA,
          llambda5,
          llambda6,
          llambda7,
          llambda8,
          aalpha3m10_mtjh,
          aalpha3m12_mtjh,
          aalpha3mage10,
          aalpha3mage12,
          ddelta4,
          MS1_10,
          VS1_10,
          MS2_10,
          VS2_10,
          MS3_10,
          VS3_10,
          MS4_10,
          VS4_10,
          MS5_10,
          VS5_10,
          MS6_10,
          VS6_10,
          MS7_10,
          VS7_10,
          MS8_10,
          VS8_10,
          MS9_10,
          VS9_10,
          MS10_10,
          VS10_10,
          MS11_10,
          VS11_10,
          MS1_12,
          VS1_12,
          MS2_12,
          VS2_12,
          MS3_12,
          VS3_12,
          MS4_12,
          VS4_12,
          MS5_12,
          VS5_12,
          MS6_12,
          VS6_12,
          MS7_12,
          VS7_12,
          MS8_12,
          VS8_12,
          MS9_12,
          VS9_12,
          MS10_12,
          VS10_12,
          MS11_12,
          VS11_12,
          MS12_12,
          VS12_12,
          MS13_12,
          VS13_12,
          MS1_BIRTH,
          VS1_BIRTH,
          MS2_BIRTH,
          VS2_BIRTH,
          MS3_BIRTH,
          VS3_BIRTH,
          MS4_BIRTH,
          VS4_BIRTH,
          MS5_BIRTH,
          VS5_BIRTH,
          MS6_BIRTH,
          VS6_BIRTH,
          MS7_BIRTH,
          VS7_BIRTH,
          MS8_BIRTH,
          VS8_BIRTH,
          MS9_BIRTH,
          VS9_BIRTH,
          MS10_BIRTH,
          VS10_BIRTH,
          MS11_BIRTH,
          VS11_BIRTH,
          MS12_BIRTH,
          VS12_BIRTH,
          MS13_BIRTH,
          VS13_BIRTH,
          MS14_BIRTH,
          VS14_BIRTH,
          MS15_BIRTH,
          VS15_BIRTH,
          MS16_BIRTH,
          VS16_BIRTH,
          MS17_BIRTH,
          VS17_BIRTH,
          MS18_BIRTH,
          VS18_BIRTH,
          MS19_BIRTH,
          VS19_BIRTH,
          MS20_BIRTH,
          VS20_BIRTH,
          MS21_BIRTH,
          VS21_BIRTH,
          MS22_BIRTH,
          VS22_BIRTH,
          MS23_BIRTH,
          VS23_BIRTH,
          MS1_PG,
          VS1_PG,
          MS2_PG,
          VS2_PG,
          MS3_PG,
          VS3_PG,
          MS4_PG,
          VS4_PG,
          MS5_PG,
          VS5_PG,
          MS6_PG,
          VS6_PG,
          MS7_PG,
          VS7_PG,
          MS8_PG,
          VS8_PG,
          MS1_BARG,
          VS1_BARG,
          MS2_BARG,
          VS2_BARG,
          MS3_BARG,
          VS3_BARG,
          MS4_BARG,
          VS4_BARG,
          MS5_BARG,
          VS5_BARG,
          MS6_BARG,
          VS6_BARG,
          MS7_BARG,
          VS7_BARG,
          MS8_BARG,
          VS8_BARG,
          MS9_BARG,
          VS9_BARG,
          MS10_BARG,
          VS10_BARG,
          MS11_BARG,
          VS11_BARG,
          MS12_BARG,
          VS12_BARG,
          MS13_BARG,
          VS13_BARG,
          MS14_BARG,
          VS14_BARG,
          MS15_BARG,
          VS15_BARG,
          MS16_BARG,
          VS16_BARG,
          MS17_BARG,
          VS17_BARG,
          MS18_BARG,
          VS18_BARG,
          MS19_BARG,
          VS19_BARG,
          MS1_INV10,
          VS1_INV10,
          MS2_INV10,
          VS2_INV10,
          MS3_INV10,
          VS3_INV10,
          MS4_INV10,
          VS4_INV10,
          MS5_INV10,
          VS5_INV10,
          MS6_INV10,
          VS6_INV10,
          MS7_INV10,
          VS7_INV10,
          MS8_INV10,
          VS8_INV10,
          MS1_INV12,
          VS1_INV12,
          MS2_INV12,
          VS2_INV12,
          MS3_INV12,
          VS3_INV12,
          MS4_INV12,
          VS4_INV12,
          MS5_INV12,
          VS5_INV12,
          MS6_INV12,
          VS6_INV12,
          MS7_INV12,
          VS7_INV12,
          MS8_INV12,
          VS8_INV12,
          MS9_INV12,
          VS9_INV12,
          MS10_INV12,
          VS10_INV12,
          MS11_INV12,
          VS11_INV12,
          MS12_INV12,
          VS12_INV12,
          MS13_INV12,
          VS13_INV12,
          MS14_INV12,
          VS14_INV12,
          MS15_INV12,
          VS15_INV12,
          MS16_INV12,
          VS16_INV12,
          MS17_INV12,
          VS17_INV12,
          MS18_INV12,
          VS18_INV12,
          MS19_INV12,
          VS19_INV12,
          MS20_INV12,
          VS20_INV12,
          MS21_INV12,
          VS21_INV12,
          MS1_EF10,
          VS1_EF10,
          MS2_EF10,
          VS2_EF10,
          MS3_EF10,
          VS3_EF10,
          MS4_EF10,
          VS4_EF10,
          MS5_EF10,
          VS5_EF10,
          MS6_EF10,
          VS6_EF10,
          MS1_EF12,
          VS1_EF12,
          MS2_EF12,
          VS2_EF12,
          MS3_EF12,
          VS3_EF12,
          MS4_EF12,
          VS4_EF12,
          MS5_EF12,
          VS5_EF12,
          MS6_EF12,
          VS6_EF12,
          MS7_EF12,
          VS7_EF12,
          MS8_EF12,
          VS8_EF12,
          MS9_EF12,
          VS9_EF12,
          MS10_EF12,
          VS10_EF12,
          MS11_EF12,
          VS11_EF12,
          MS12_EF12,
          VS12_EF12,
          MS13_EF12,
          VS13_EF12,
          MS14_EF12,
          VS14_EF12,
          VPRICE1)

estimar=1
if(estimar==1){
  #Large running version is the following one:
  #hessdirectly<-abs(hessian(func=F_likelihood_FIN,x=partest,method="Richardson",method.args=list(r=10)))
  #This one is the faster hopefully
  hessdirectly<-abs(hessian(func=F_likelihood_FIN,x=par,method.args=list(r=4)))
  write.csv(hessdirectly,file="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/ParameterEstimates/HESSDIRECTLY2.csv")
  
  #Problem when inverting is that does parameters that do not move the hessian are not invertible
 
  hess2<-hessdirectly[1:78,1:78]
  invhess<-solve(hess2)
  VARCOVFIN=sqrt(abs(invhess))
  VARCOVFIN<-diag(VARCOVFIN)
}

#Problem i



write.csv(VARCOVFIN,file="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/ParameterEstimates/VARCOVFAST.csv")
#Check lecture 6 of Xu-cheng's class. Inverse of hessian is variance-covariance matrix. 

read.csv(VARCOV,file="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL68/ParameterEstimates/VARCOVFAST.csv")
algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL68/ParameterEstimates/VARCOV2.csv", sep=",", header=TRUE)


read.csv(VARCOV,file="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/ParameterEstimates/VARCOVFAST.csv")
algo<-read.table("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL72/ParameterEstimates/VARCOV2.csv", sep=",", header=TRUE)


st=algo[,2]

#table=data.frame(parTITLES,par,hess)
#st=abs(par/(8.5)+rnorm(325,0,0.05))
#st=VARCOV


#Trying this: par<-partest. It might not work, make it back again
par<-partest


table=data.frame(parTITLES,par,st)
colnames(table)<-c("Parameter","Estimate","Standard Error")
DT=3
tabledir="/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/advances/reports/ParameterEstimates"

#=============================================#
#I will divide the results in different tables#
#=============================================#

#=====================================#
#  Mother's preferences               #
#=====================================#

DIGITSHERE=4

comptable<-rbind(table[1:5,],table[48:51,],table[53,],table[52,])

setwd(tabledir)
estimates1<-xtable(comptable,caption="Estimates:  Utility function. Mother's preferences",digits=DIGITSHERE,table.placement="H",label="tab:EstimatesMothPref")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMothPref.tex")



#====================#
#Father's preferences#
#====================#
setwd(tabledir)
comptable<-rbind(table[26:30,],table[54:57,],table[59,],table[58,])
estimates1<-xtable(comptable,caption="Estimates:  Utility function. Father's preferences",digits=DIGITSHERE,table.placement="H",label="tab:EstimatesFathPref")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesFathPref.tex")




#=============================#
#     Production of skills#
#=============================#
#Include columnt to specify explanation of the parameters

DT=4
setwd(tabledir)
#Generating the compound table:

comptable<-rbind(table[6:9,],table[11:12,],table[21:25,],table[78,],table[10,])

estimates1<-xtable(comptable,caption="Estimates:  Production of Skills",digits=DT,table.placement="H",label="tab:EstimatesSkillsProd")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesSkillsProd.tex")


#Nocaption
estimates1<-xtable(comptable,digits=DT,table.placement="H",label="tab:EstimatesSkillsProd")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesSkillsProdNOCAPTION.tex")

#==============#
#Mother's Wages#
#==============#

setwd(tabledir)
estimates1<-xtable(table[13:17,],caption="Estimates:  Mothers wages",digits=DT,table.placement="H",label="tab:EstimatesMWages")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMWages.tex")


#=============#
#Father's wage#
#=============#
setwd(tabledir)
estimates1<-xtable(table[31:35,],caption="Estimates:  Fathers wages",digits=DT,table.placement="H",label="tab:EstimatesFWages")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesFWages.tex")



#=============#
#Pareto weight#
#=============#
Description<-matrix(0,10,1)
Description[1,1]="Intercept"
Description[2,1]="Wage ratio"
Description[3,1]="Non-labor income ratio"
Description[4,1]="Age difference"
Description[5,1]="Educational difference"
Description[6,1]="Gender ratio"
Description[7,1]="Unemployment ratio"
Description[8,1]="Wage ratio (province)"
Description[9,1]="Distance to center"
Description[10,1]="Standard deviation"

comptable<-rbind(table[37:41,],table[70:73,],table[42,])
comptable=cbind(comptable,Description)
setwd(tabledir)
estimates1<-xtable(comptable,caption="Estimates:  Pareto weight",digits=DT,table.placement="H",label="tab:EstimatesPWeight")
align(estimates1)<-"ccccl"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesPWeight.tex")




#=================#
#Preference shocks#
#=================#

#We can at most identify the sum of shocks. We have to set one to something. 


copmtable<-rbind(table[62:63,],table[65:67,],table[69,])
setwd(tabledir)

comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(copmtable))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            "\\multicolumn{3}{l}{Preference shocks for work-no childcare are standardized to zero }\\\\ \n",
                            sep = ""))


estimates1<-xtable(copmtable,caption="Estimates: Preference shock",digits=DT,table.placement="H",label="tab:EstimatesPSHOCKS")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),table.placement="H", sanitize.text.function=function(x){x},add.to.row = comment,hline.after = c(-1, 0)),file="EstimatesPSHOCKS.tex")



#=======================#
#Distribution of factors#
#=======================#
setwd(tabledir)
comptable<-rbind(table[18,],table[36,],table[19,])
estimates1<-xtable(comptable,caption="Estimates: Distribution of latent factors",digits=DT,table.placement="H",label="tab:EstimatesVarFactors")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesVarFactors.tex")



#=======================#
#        Prices         #
#=======================#
setwd(tabledir)
comptable<-rbind(table[20,],table[325,],table[60,],table[61,])
estimates1<-xtable(comptable,caption="Estimates:  Prices",digits=DT,table.placement="H",label="tab:EstimatesPrices")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesPrices.tex")



#===============================#
#Measurement systems-Skills 2010#
#===============================#

#For the standardization I will use the factor with the
#largest loading


setwd(tabledir)

#Factor loading standardized
table[79:100,]$Estimate=-table[79:100,]$Estimate/table[99,]$Estimate

#Standard error standardized
table[79:100,3]=-table[79:100,3]/table[99,]$Estimate

#Standardized factor should not have se. 
table[99,3]=0


comptable<-rbind(table[79:84,],table[87:100,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Skills in 2010",digits=DT,table.placement="H",label="tab:EstimatesMeasureSkills2010")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureSkills2010.tex")


#===============================#
#Measurement systems-Skills 2012#
#===============================#

setwd(tabledir)
#Normalizing first factor to one:

#Factor loading standardized
table[101:126,]$Estimate=table[101:126,]$Estimate/table[101,]$Estimate

#Standard error standardized
table[101:126,3]=table[101:126,3]/table[101,]$Estimate

#Standardized factor should not have se. 
table[101,3]=0


#Generating table
comptable<-rbind(table[101:120,],table[125:126,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Skills in 2012",digits=DT,table.placement="H",label="tab:EstimatesMeasureSkills2012")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureSkills2012.tex")


#===================================#
#Measurement systems-Skills at birth#
#===================================#

setwd(tabledir)


#Normalizing first factor to one:

#Factor loading standardized
table[127:172,]$Estimate=-table[127:172,]$Estimate/table[151,]$Estimate

#Standard error standardized
table[127:172,3]=-table[127:172,3]/table[151,]$Estimate

#Standardized factor should not have se. 
table[151,3]=0

comptable<-rbind(table[127:172,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Skills at birth",digits=DT,table.placement="H",label="tab:EstimatesMeasureSkillsBirth")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureSkillsBirth.tex")



#===============================================#
#Measurement systems-Skills of primary Caregiver#
#===============================================#


#Normalizing first factor to one:

#Factor loading standardized
table[173:188,]$Estimate=table[173:188,]$Estimate/table[179,]$Estimate

#Standard error standardized
table[173:188,3]=table[173:188,3]/table[179,]$Estimate

#Standardized factor should not have se. 
table[179,3]=0

setwd(tabledir)
comptable<-rbind(table[173:186,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Skills of Primary Caregiver",digits=DT,table.placement="H",label="tab:EstimatesMeasureSkillsPG")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureSkillsPG.tex")


#===============================#
#Pareto weight#
#===============================#

#Normalizing first factor to one:
comptable<-rbind(table[189:216,],table[219:226,])

#Factor loading standardized
comptable[,2]=comptable$Estimate/table[223,]$Estimate

#Standard error standardized
table[223,3]=0
comptable[,3]=comptable[,3]/table[223,]$Estimate

#Standardized factor should not have se. 


setwd(tabledir)
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Pareto weight",digits=DT,table.placement="H",label="tab:EstimatesMeasureBARG")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureBARG.tex")


#============================#
#Table of investments in 2010#
#============================#



#Normalizing first factor to one:

#Factor loading standardized
table[227:242,]$Estimate=table[227:242,]$Estimate/table[229,]$Estimate

#Standard error standardized
table[227:242,3]=table[227:242,3]/table[229,]$Estimate

#Standardized factor should not have se. 
table[229,3]=0



setwd(tabledir)
comptable<-rbind(table[227:242,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Investments 2010",digits=DT,table.placement="H",label="tab:EstimatesMeasureINV2010")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureINV2010.tex")


#============================#
#Table of investments in 2012#
#============================#


#Normalizing first factor to one:

#Factor loading standardized
table[243:284,]$Estimate=table[243:284,]$Estimate/table[279,]$Estimate

#Standard error standardized
table[243:284,3]=table[243:284,3]/table[279,]$Estimate

#Standardized factor should not have se. 
table[279,3]=0


setwd(tabledir)
comptable<-rbind(table[243:284,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Investments 2012",digits=DT,table.placement="H",label="tab:EstimatesMeasureINV2012")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureINV2012.tex")

#================================#
#Table of Parental effort in 2010#
#================================#



#Normalizing first factor to one:

#Factor loading standardized
table[285:296,]$Estimate=table[285:296,]$Estimate/table[295,]$Estimate

#Standard error standardized
table[285:296,3]=table[285:296,3]/table[295,]$Estimate

#Standardized factor should not have se. 
table[295,3]=0


setwd(tabledir)
comptable<-rbind(table[285:296,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Parental effort 2010",digits=DT,table.placement="H",label="tab:EstimatesMeasureEFF2010")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureEFF2010.tex")


#================================#
#Table of Parental effort in 2012#
#================================#


#Normalizing first factor to one:

#Factor loading standardized
table[297:324,]$Estimate=table[297:324,]$Estimate/table[323,]$Estimate

#Standard error standardized
table[297:324,3]=table[297:324,3]/table[323,]$Estimate

#Standardized factor should not have se. 
table[323,3]=0


setwd(tabledir)
comptable<-rbind(table[297:324,])
estimates1<-xtable(comptable,caption="Estimates:  Measurement system -Parental effort 2012",digits=DT,table.placement="H",label="tab:EstimatesMeasureEFF2012")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),  table.placement="H", sanitize.text.function=function(x){x}),file="EstimatesMeasureEFF2012.tex")




#========#
#Table 14#
#========#
setwd(tabledir)
estimates1<-xtable(table[74:77,],caption="Estimates:  Utility function. Mother's preferences",digits=DT,table.placement="H",label="tab:Estimates14")
align(estimates1)<-"cccc"
write(print(estimates1,include.rownames=(FALSE),   table.placement="H",sanitize.text.function=function(x){x}),file="Estimates14.tex")


