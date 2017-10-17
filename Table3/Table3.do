*==================*
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/FiguresandTables"
use temp.dta, clear
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/FiguresandTables/SUMMARYSTATISTICS"
g Htotalincomeweek12=Htotalincome12/4.3
global SUMSTATS "Mage12  Fage12 Myrschool12 Fyrschool12 Mhourswork2_12 Fhourswork2_12 Mwage12INC Mwage12DOLLARS  Fwage12INC  Fwage12DOLLARS Htotalincomeweek12 Htotalincomeweek12DOLLARS  Cedad_meses12"


*Hourly wages

replace Fwage12=Fwage12*((1+0.017)*(1+0.033)) //Inflation adj
replace Mwage12=Mwage12*((1+0.017)*(1+0.033)) //Inflation adj
replace Fwage12=Fwage12
replace Mwage12=Mwage12

*Before it was /1000. Now it is /45 because rather than doing weekly -thousands of pesos, we are doing hourly wage. 

*label var Fwage12 "Father's wage (Weekly-Chilean pesos thousands)"
*label var Mwage12 "Mother's wage (Weekly-Chilean Pesos thousands)"

*We need to be carefuel with Fwage12 because it includes zeros for people not working!
g Fwage12INC=Fwage12 if Ffraclabor12!=0
g Mwage12INC=Mwage12 if Mfraclabor12!=0


label var Fwage12INC "Father's weekly wage (1,000 CLP)"
label var Mwage12INC "Mother's weekly wage (1,000 CLP)"

*In dollars
g Mwage12DOLLARS= Mwage12INC*0.002
g Fwage12DOLLARS= Fwage12INC*0.002

replace Mwage12INC = Mwage12INC/1000
replace Fwage12INC = Fwage12INC/1000

label var Mwage12DOLLARS "Mother's weekly wage (USD)"
label var Fwage12DOLLARS "Father's weekly wage (USD)"

label var Fage12 "Father's age"
label var Mage12 "Mother's age"
label var Cedad_meses12 "Age of child (months)"
label var Myrschool12 "Mother's years of schooling"
label var Fyrschool12 "Father's years of schooling"
label var Mhourswork2_12 "Mother's hours of work (week)"
label var Fhourswork2_12 "Father's hours of work (week)"
label var Htotalincomeweek12 "Household's total Income (Weekly-CLP)"

g Htotalincomeweek12DOLLARS= Htotalincomeweek12*0.002 
label var Htotalincomeweek12DOLLARS "Household's total Income (Weekly (USD))"
replace Htotalincomeweek12= Htotalincomeweek12/1000

sutex $SUMSTATS, label digits(2) par placement(H) file("SUMMARYSTATISTICS.tex") key("tab:Summarystatistics")  min replace
sutex $SUMSTATS, label digits(2) par placement(H)  key("tab:Summarystatistics")
sutex $SUMSTATS, label digits(2) par placement(H)  key("tab:Summarystatistics") min

