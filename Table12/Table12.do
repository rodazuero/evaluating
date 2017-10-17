
*====================*
*Distribution factors*
*====================*
use Table12.dta
global DIST "ymratio Agedif Edudif FMRATIO Unemployment Wageratio"
label var ymratio "Father's non-labor income share"
label var Agedif "Age difference (Father-Mother)"
label var Edudif "Difference in grades attained (Father-Mother)"
label var FMRATIO "Sex ratio in region (Women/Men)"
label var Unemployment "Unemployment ratio in region (Men/Women)"
label var Wageratio "Wage ratio in region (Men/Women)"
sutex $DIST, label title("Summary statistics-Variables determining Pareto weight") digits(2) par placement(H) file("DISTRIBUTIONFACTORSDESCRIPTIVE.tex") key("tab:DISTRIBUTIONFACTORSDESCRIPTIVE") replace
sutex $DIST, label title("Summary statistics-Variables determining Pareto weight") digits(2) par placement(H)  key("tab:DISTRIBUTIONFACTORSDESCRIPTIVE")

