

*0 Verification PRE. Filter one: children who are in both samples in total. 
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/ELPI_2010_2014/Bases_Públicas/ELPI_2010/Data/Entrevistada_2010.dta", clear
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/ELPI_2010_2014/Bases_Públicas/ELPI_2012/Data/Entrevistada_2012.dta"
count

*You will see that this number coincides with those that have Cliveswithfather10==. and Cliveswithfather12==. 
*----------------------------------------------
*1. Start with the countdown. TOTAL
*----------------------------------------------


use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/RR/BEHAVIORAL70/BEHAVIORALFINAL.dta", clear
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/modified/mergetotal.dta", gen(mergeTOTAL)
count


*----------------------------------------------
*2. Children in 2010 not in 2012
*----------------------------------------------
drop if Cliveswithfather12==.
count


*----------------------------------------------
*3. Children in 2012 not in 2010:
*-----------------------------------------------
drop if Cliveswithfather10==.
count

*-----------------------------------------------
*4. Drop if does not live with either father or mother
*-----------------------------------------------

drop if Cliveswithfather12==0 | Cliveswithmother12==0 | Cliveswithfather10==0 | Cliveswithmother10==0 
count
drop _merge
save "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/FiguresandTables/SAMPLESELECTION/TEMPORAL.dta", replace


*-----------------------------------------------
*5. Siblings in the age range not corresponding
*-----------------------------------------------
use "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/bases/Hogar_2012.dta", clear
g edsib=i1 if i2==6
g edorig=i1 if i2==13
bys folio: egen edoriginal=max(edorig)
g difedad=abs(edoriginal-edsib)
bys folio: egen minedad=min(difedad)
replace minedad=99 if minedad==.
keep folio minedad
collapse (max) minedad, by(folio)
merge 1:1 folio using "/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/FiguresandTables/SAMPLESELECTION/TEMPORAL.dta"
keep if _merge==3
drop _merge
keep if minedad>4

*Dropping the ones that correspond to not having variables reported



*===================================
*5. Birth outcomes 
*===================================

keep if Ccondpregg23!=. & Ccondpregg24!=.

*===================================
*6. Childrens skills questionnaires
*===================================
keep if CSTDtepsi_pb_mot10!=. & CSTDbt_112!=.



*===============
*7. Remaining
*===============

keep if mergeTOTAL==3
