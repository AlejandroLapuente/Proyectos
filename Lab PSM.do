// Laboratorio PSM
// Andro Asatashvili, Alejandro Lapuente, Emiliano Pérez y carlos Pérez

cd "C:\Users\salon\Downloads"
use nsw_dw_new, clear

global Y re78
global TREAT treat
global CONTROL1 age education married
global CONTROL2 black hispanic nodegree re74 re75

reg $Y $TREAT
outreg2 using NSW, excel replace ctitle(Tratamiento sin control) stats(coef se ci) dec(3) nocons nodepvar keep(treat)

reg $Y $TREAT $CONTROL1
outreg2 using NSW, excel append ctitle(Tratamiento sin control) stats(coef se ci) dec(3) nocons nodepvar keep(treat)

reg $Y $TREAT $CONTROL1 $CONTROL2
outreg2 using NSW, excel append ctitle(Tratamiento sin control) stats(coef se ci) dec(3) nocons nodepvar keep(treat)

use psid_controls_new, clear

ttest re78, by(treat) reverse

ssc install psmatch2, replace

psmatch2 $TREAT $CONTROL1, outcome($Y) ate
psmatch2 $TREAT $CONTROL1 $CONTROL2, outcome($Y) ate