clear all
set mem 800m
set more off
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal.dta"

*------------------------------------------------*
*Info about how to obtain the wealth distribution*
*------------------------------------------------*

*merge 1:1 folio using 
*Talla y peso al nacer por nivel de ingresos
*Generando los quintiles de ingreso
*xtile quint=totincome, nq(5)

*======================*
*1. All-boys and girlst*
*======================*


*-------------------*
*1.1 Skills at birth*
*-------------------*

xtile quint=Hincome12  , nq(5)
replace g18=. if g18==9


*Storing sd
foreach var of varlist Ccondpregg23 Ccondpregg24  Ccondpregpreterm{
	forvalues i=1(1)5{
		sum `var' if quint==`i'
		local `var'q`i'=r(sd)/sqrt(r(N))
		di as text "-------------------"
		di as text "variable -> `var'"
		di as text "quintil -> `i'"
		di as text "varqi->``var'q`i''"
		}
	}


collapse (mean) Ccondpregg23 (mean) Ccondpregg24 (mean) Ccondpregpreterm (mean) g18, by(quint)  
*replace Ccondpregpreterm = Ccondpregpreterm*100

*Lower confidence interval
foreach var of varlist Ccondpregg23 Ccondpregg24  Ccondpregpreterm{
	g `var'LL=.
	g `var'UU=.
	forvalues i=1(1)5{
		replace `var'LL=`var'-1.96*``var'q`i'' in `i'
		replace `var'UU=`var'+1.96*``var'q`i'' in `i'
		di as text "-------------------"
		di as text "variable -> `var'"
		di as text "quintil -> `i'"
		di as text "varqi->``var'q`i''"
		}
	}


local CICOLOR="blue"


replace Ccondpregpreterm=100*Ccondpregpreterm
replace CcondpregpretermLL=100*CcondpregpretermLL
replace CcondpregpretermUU=100*CcondpregpretermUU

local CICOLOR="blue"
#d ;
twoway (connected Ccondpregg23 quint ,ylabel(49(0.5)51, angle(0)) xtitle("Income quintile") ytitle("cm") title("Height at birth"))
	(connected Ccondpregg23LL quint, lpattern(dash)  color(`CICOLOR'))
	(connected Ccondpregg23UU quint, lpattern(dash)  color(`CICOLOR'))
		, name(g1,replace) scheme(s1color) legend(off);
	
#d ;		
twoway (connected Ccondpregg24 quint , ylabel(3.35(0.05)3.55, angle(0) format(%9.1f)) xtitle("Income quintile") ytitle("Kg") title("Weight at birth"))
	(connected Ccondpregg24LL quint, lpattern(dash)  color(blue))
	(connected Ccondpregg24UU quint, lpattern(dash)  color(blue))
		, name(g2,replace) scheme(s1color) legend(off);

#d ;	
twoway (connected Ccondpregpreterm quint , ylabel(7(0.5)12, angle(0)) xtitle("Income quintile") ytitle("%") title("Pre-term births"))
	(connected CcondpregpretermLL quint, lpattern(dash)  color(blue))
	(connected CcondpregpretermUU quint, lpattern(dash)  color(blue))
		, name(g3,replace) scheme(s1color) legend(off);

#d cr
*twoway (connected Ccondpregg23 quint ,ylabel(45(1)55, angle(0)) xtitle("Income quintile") ytitle("cm") title("Height at birth")), name(g1,replace) scheme(s1color)
*twoway (connected Ccondpregg24 quint , ylabel(3(0.5)4, angle(0)) xtitle("Income quintile") ytitle("Kg") title("Weight at birth")), name(g2,replace) scheme(s1color)
*twoway (connected Ccondpregpreterm quint , ylabel(1(1)10, angle(0)) xtitle("Income quintile") ytitle("%") title("Pre-term births")), name(g3,replace) scheme(s1color)
graph combine g1 g2 g3, c(3) fysize(60) scheme(s1color) graphregion(color(white)) title(" ")
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/advances/reports/Figures"
graph export gapsBIRTHALL.pdf,  as(pdf) replace




*------------------------*
*1.2 Skills 5 years later*
*------------------------*
clear all
set more off
set mem 800m
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal12.dta"
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/bp_evaluaciones.dta", gen(mergete)
drop merget
xtile quint=Hincome12  , nq(5)
tab quint, gen(quintiles)



*Need to standardize tests
global TESTS "Ctvip_pt12 bt_1 bt_2 bt_3  bt_4 bt_5  Ctadi_pt_cog12 Ctadi_pt_len12 Ctadi_pt_mot12 Ctadi_pt_se12 Ctadi_pt_total12"
foreach var of global TESTS{
	sum `var'
	replace `var'=(`var'-r(mean))/r(sd)
	}
	
*Storing sd
foreach var of global TESTS{
	forvalues i=1(1)5{
		sum `var' if quint==`i'
		local `var'q`i'=r(sd)/sqrt(r(N))
		}
	}

collapse peso_nino talla_nino $TESTS , by(quint)


*Lower confidence interval
foreach var of global TESTS{
	g `var'LL=.
	g `var'UU=.
	forvalues i=1(1)5{
		replace `var'LL=`var'-1.96*``var'q`i'' in `i'
		replace `var'UU=`var'+1.96*``var'q`i'' in `i'
		}
	}

local sizetitle="large"
local sizexL ="large"
local sizeyL ="large"
local labelY="large"
local labelX="huge"

*Color for confidence interval
local CICOLOR="blue"

#d ;
twoway (connected Ctvip_pt12 quint , ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL')) title("PPVT", size(`sizetitle')))
	(connected Ctvip_pt12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctvip_pt12UU quint, lpattern(shortdash)  color(`CICOLOR'))
		, scheme(s1color) legend(off);


twoway (connected Ctvip_pt12 quint , ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL')) title("PPVT", size(`sizetitle')))
(connected Ctvip_pt12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctvip_pt12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g1, replace) scheme(s1color)  legend(off);



twoway (connected bt_1 quint ,  ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("Battelle: adaptative", size(`sizetitle')))
(connected bt_1LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_1UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g2, replace) scheme(s1color)  legend(off);
	
	
	
twoway (connected bt_2 quint , ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("Battelle: Cognitive", size(`sizetitle')))
(connected bt_2LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_2UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g3, replace) scheme(s1color)  legend(off);
	
	
twoway (connected bt_3 quint , ylabel(-0.2(0.1)0.3,angle(0)  labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL')) title("Battelle: Communication", size(`sizetitle')))
(connected bt_3LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_3UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g4, replace) scheme(s1color)  legend(off);
	
	
twoway (connected bt_4 quint, ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("Battelle: Motor skills", size(`sizetitle')))
(connected bt_4LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_4UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g5, replace) scheme(s1color)  legend(off);
	
	
twoway (connected bt_5 quint , ylabel(-0.2(0.1)0.3,angle(0)  labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("Battelle: Personal social", size(`sizetitle')))
(connected bt_5LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_5UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g6, replace) scheme(s1color)  legend(off);
	
	
twoway (connected Ctadi_pt_cog12 quint , ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL')) title("TADI: Cognitive test", size(`sizetitle')))
(connected Ctadi_pt_cog12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctadi_pt_cog12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g7, replace) scheme(s1color)  legend(off);
	
	
twoway (connected Ctadi_pt_len12 quint, ylabel(-0.2(0.1)0.3,angle(0)  labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL')) title("TADI: Language test", size(`sizetitle')))
(connected Ctadi_pt_len12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctadi_pt_len12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g8, replace) scheme(s1color)  legend(off);
	
	
twoway (connected Ctadi_pt_mot12 quint , ylabel(-0.2(0.1)0.3,angle(0)  labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("TADI: Motor skills", size(`sizetitle')))
(connected Ctadi_pt_mot12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctadi_pt_mot12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g9, replace) scheme(s1color)  legend(off);
	
	
twoway (connected Ctadi_pt_se12 quint , ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("TADI: Socioemotinal test", size(`sizetitle')))
(connected Ctadi_pt_se12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctadi_pt_se12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g10, replace) scheme(s1color)  legend(off);
	
	
twoway (connected Ctadi_pt_total12 quint , ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("TADI: Total development test", size(`sizetitle')))
(connected Ctadi_pt_total12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctadi_pt_total12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g11, replace) scheme(s1color)  legend(off);
	
#d cr
	
	
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/advances/reports/Figures"
graph combine g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11, r(3)  name(gapsOLD, replace) graphregion(color(white)) scheme(s1color) title(" ")
graph export gapsOLDALLALLTESTS.pdf,  as(pdf) replace
graph combine g1 g3 g10 g5 , c(2) fysize(100) name(gapsOLD, replace) graphregion(color(white)) scheme(s1color)  title(" ")
graph export gapsOLDALL.pdf,  as(pdf) replace



*Name for legend
local sizetitle="large"
local sizexL ="large"
local sizeyL ="large"
local labelY="large"
local labelX="huge"

*Color for confidence interval
local CICOLOR="blue"
local INCOLOR ="green"
#d ;
twoway (connected Ctvip_pt12 quint , color(`INCOLOR') ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY') ) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL')) title("PPVT: Vocabulary test", color(black) size(`sizetitle')))
(connected Ctvip_pt12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctvip_pt12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g1, replace)   legend(order(1 "Mean" 2 "95% Confidence Interval")) graphregion(color(white));

twoway (connected bt_2 quint ,  color(`INCOLOR') ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("Battelle: Cognitive",  color(black) size(`sizetitle')))
(connected bt_2LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_2UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g3, replace)  legend(off) graphregion(color(white));
	
	twoway (connected bt_4 quint,  color(`INCOLOR') ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("Battelle: Motor skills",  color(black) size(`sizetitle')))
(connected bt_4LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected bt_4UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g5, replace) legend(off) graphregion(color(white));
	
twoway (connected Ctadi_pt_se12 quint ,  color(`INCOLOR') ylabel(-0.2(0.1)0.3, angle(0) labs(`labelY')) xlabel(1(1)5, labs(`labelY')) xtitle("Income quintile",  size(`sizexL')) ytitle("sd",  size(`sizeyL'))  title("TADI: Socioemotional test", color(black) size(`sizetitle')))
(connected Ctadi_pt_se12LL quint, lpattern(shortdash) color(`CICOLOR'))
	(connected Ctadi_pt_se12UU quint, lpattern(shortdash)  color(`CICOLOR')), name(g10, replace)   legend(off) graphregion(color(white));
	
	
	
#d cr
grc1leg g1 g3 g10 g5 , c(2) fysize(120) name(gapsOLD, replace) graphregion(color(white)) scheme(s1color)  title(" ") legendfrom(g1)
graph export gapsOLDALLPPT.pdf,  as(pdf) replace

factor Ctvip_pt12 bt_1 bt_2 bt_3 bt_4 bt_5, factor(1)
predict factoria
*============================================================*
*============================================================*
*============================================================*
*============================================================*
*============================================================*
*============================================================*
*============================================================*


*==========================*
*Doing it by gender        *
*==========================*

*========*
*2. Boys *
*========*



*-------------------*
*2.1 Skills at birth*
*-------------------*

clear all
set more off
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal.dta"
keep if Cgender12==1
*merge 1:1 folio using 
*Talla y peso al nacer por nivel de ingresos
*Generando los quintiles de ingreso
*xtile quint=totincome, nq(5)
xtile quint=Hincome12  , nq(5)

replace g18=. if g18==9

collapse Ccondpregg23 Ccondpregg24 Ccondpregpreterm g18, by(quint)  
replace Ccondpregpreterm = Ccondpregpreterm*100
twoway (connected Ccondpregg23 quint ,ylabel(45(1)55) xtitle("Income quintile") ytitle("cm") title("Height at birth")), name(g1,replace) scheme(s1color)
twoway (connected Ccondpregg24 quint , ylabel(3(0.5)4) xtitle("Income quintile") ytitle("Kg") title("Weight at birth")), name(g2,replace) scheme(s1color)
twoway (connected Ccondpregpreterm quint , ylabel(1(1)10) xtitle("Income quintile") ytitle("%") title("Pre-term births")), name(g3,replace) scheme(s1color)
graph combine g1 g2 g3, c(3) fysize(60) scheme(s1color) graphregion(color(white))  title("Skills at birth-Boys")
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/emergenceofgaps"
graph export gapsBIRTHBOYS.pdf,  as(pdf) replace


*---------------------------*
*2.2 Skills five years later*
*---------------------------*


clear all
set more off
set mem 800m
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal12.dta"
keep if Cgender12==1
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/bp_evaluaciones.dta", gen(mergete)
keep if Cgender12==1
drop merget
xtile quint=Hincome12  , nq(5)
tab quint, gen(quintiles)

*Need to standardize tests
global TESTS "Ctvip_pt12 bt_1 bt_2 bt_3  bt_4 bt_5  Ctadi_pt_cog Ctadi_pt_len Ctadi_pt_mot Ctadi_pt_se Ctadi_pt_total"
foreach var of global TESTS{
	sum `var'
	replace `var'=(`var'-r(mean))/r(sd)
	}

collapse peso_nino talla_nino $TESTS , by(quint)


twoway (connected Ctvip_pt12 quint , ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("PPVT")), name(g1, replace) scheme(s1color)
twoway (connected bt_1 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: adaptative")), name(g2, replace) scheme(s1color)
twoway (connected bt_2 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Cognitive")), name(g3, replace) scheme(s1color)
twoway (connected bt_3 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Communication")), name(g4, replace) scheme(s1color)
twoway (connected bt_4 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Motor skills")), name(g5, replace) scheme(s1color)
twoway (connected bt_5 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Personal social")), name(g6, replace) scheme(s1color)
twoway (connected Ctadi_pt_cog quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Cognitive test")), name(g7, replace) scheme(s1color)
twoway (connected Ctadi_pt_len quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Language test at birth")), name(g8, replace) scheme(s1color)
twoway (connected Ctadi_pt_mot quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Motor skills")), name(g9, replace) scheme(s1color)
twoway (connected Ctadi_pt_se quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Socioemotinal test")), name(g10, replace) scheme(s1color)
twoway (connected Ctadi_pt_total quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Total development test")), name(g11, replace) scheme(s1color)


cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/emergenceofgaps"


graph combine g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11, r(3) title("Skills-5 years old boys") name(gapsOLDALL) graphregion(color(white)) scheme(s1color)
graph export gapsOLDBOYSALLTESTS.pdf,  as(pdf) replace
graph combine g1 g3 g4 g5 , c(4) fysize(60) name(gapsOLD, replace) graphregion(color(white)) scheme(s1color) title("Skills-5 years old boys")
graph export gapsOLDBOYS.pdf,  as(pdf) replace



*================
*3. Girls
*================



*-------------------*
*3.1 Skills at birth*
*-------------------*



clear all
set more off
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal.dta"
keep if Cgender12==0
xtile quint=Hincome12  , nq(5)

replace g18=. if g18==9

collapse Ccondpregg23 Ccondpregg24 Ccondpregpreterm g18, by(quint)  
replace Ccondpregpreterm = Ccondpregpreterm*100

twoway (connected Ccondpregg23 quint ,ylabel(45(1)55) xtitle("Income quintile") ytitle("cm") title("Height at birth")), name(g1,replace) scheme(s1color)

twoway (connected Ccondpregg24 quint , ylabel(3(0.5)4) xtitle("Income quintile") ytitle("Kg") title("Weight at birth")), name(g2,replace) scheme(s1color)
twoway (connected Ccondpregpreterm quint , ylabel(1(1)10) xtitle("Income quintile") ytitle("%") title("Pre-term births")), name(g3,replace) scheme(s1color)
graph combine g1 g2 g3, c(3) fysize(60) scheme(s1color) graphregion(color(white)) title("Skills at birth-Girls") 
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/emergenceofgaps"
graph export gapsBIRTHGIRLS.pdf,  as(pdf) replace


*-------------------------*
*3.2 Girls 5 years later  *
*-------------------------*

clear all
set more off
set mem 800m
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal12.dta"
keep if Cgender12==0
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/bp_evaluaciones.dta", gen(mergete)
keep if Cgender12==0
drop merget
xtile quint=Hincome12  , nq(5)
tab quint, gen(quintiles)

*Need to standardize tests
global TESTS "Ctvip_pt12 bt_1 bt_2 bt_3  bt_4 bt_5  Ctadi_pt_cog Ctadi_pt_len Ctadi_pt_mot Ctadi_pt_se Ctadi_pt_total"
foreach var of global TESTS{
	sum `var'
	replace `var'=(`var'-r(mean))/r(sd)
	}

collapse peso_nino talla_nino $TESTS , by(quint)


twoway (connected Ctvip_pt12 quint , ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("PPVT")), name(g1, replace) scheme(s1color)
twoway (connected bt_1 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: adaptative")), name(g2, replace) scheme(s1color)
twoway (connected bt_2 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Cognitive")), name(g3, replace) scheme(s1color)
twoway (connected bt_3 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Communication")), name(g4, replace) scheme(s1color)
twoway (connected bt_4 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Motor skills")), name(g5, replace) scheme(s1color)
twoway (connected bt_5 quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("Battelle: Personal social")), name(g6, replace) scheme(s1color)
twoway (connected Ctadi_pt_cog quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Cognitive test")), name(g7, replace) scheme(s1color)
twoway (connected Ctadi_pt_len quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Language test at birth")), name(g8, replace) scheme(s1color)
twoway (connected Ctadi_pt_mot quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Motor skills")), name(g9, replace) scheme(s1color)
twoway (connected Ctadi_pt_se quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Socioemotinal test")), name(g10, replace) scheme(s1color)
twoway (connected Ctadi_pt_total quint ,  ylabel(-0.2(0.1)0.3) xtitle("Income quintile") ytitle("sd") title("TADI: Total development test")), name(g11, replace) scheme(s1color)

cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/emergenceofgaps"
graph combine g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11, r(3) title("Skills-5 years old girls") name(gapsOLDALL) graphregion(color(white)) scheme(s1color)
graph export gapsOLDGIRLSALLTESTS.pdf,  as(pdf) replace
graph combine g1 g3 g4 g5 , c(4) fysize(60) name(gapsOLD, replace) graphregion(color(white)) scheme(s1color) title("Skills-5 years old girls")
graph export gapsOLDGIRLS.pdf,  as(pdf) replace



*--------*
*TVIP RAW*
*--------*


clear all
set more off
set mem 800m
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal12.dta"
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/bp_evaluaciones.dta", gen(mergete)
drop merget
xtile quint=Hincome12  , nq(5)
tab quint, gen(quintiles)



*Need to standardize tests
global TESTS "Ctvip_pt12 bt_1 bt_2 bt_3  bt_4 bt_5  Ctadi_pt_cog Ctadi_pt_len Ctadi_pt_mot Ctadi_pt_se Ctadi_pt_total"
foreach var of global TESTS{
	sum `var'
	replace `var'=(`var'-r(mean))/r(sd)
	}

drop if quint==.
drop if Ctvip_pb12==.

*I will store the dataset in CSV as an attempt of doing it in R for the motivation of the talk
keep $TESTS peso_nino talla_nino Ctvip_pb12 quint
cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/emergenceofgaps"
outsheet using "testsmotivation.csv", comma replace

keep if quint==5
pctile pct= Ctvip_pb12,nq(100)


collapse peso_nino talla_nino $TESTS  Ctvip_pb12, by(quint)

cd "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/advances/reports/Figures"
twoway (connected Ctvip_pb12 quint , xtitle("Income quintile") ytitle("Number of Words Recognized") title("Peabody Picture Vocabulary Test")), name(g12, replace) scheme(s1color)
graph export tvipraw,  as(pdf) replace
*graph export gapsOLDALL.pdf,  as(pdf) replace







