*****************
** Examen 2022 **
*****************

cd "/Users/alejandrolapuente/Desktop/Alejandro/CIDE/7mo semestre/Microeconometrics/Examen 2022"
use MalariaData_reduced, clear

global Y m_care_chem m_care_nothing m_care_hc m_took_maltest m_took_act
//global TREAT control act40 act60 act100
global X B_knowledge_correct B_head_edu B_head_age_imputed B_dist_km

gen groups = .
replace groups = 0 if control ==1
replace groups = 1 if act40 ==1
replace groups = 2 if act60 ==1
replace groups = 3 if act100 ==1
label define groups 0 "Control" 1 "Act 40" 2 "Act 60" 3 "Act 100"


foreach var in $Y{
	reg `var' i.groups
	outreg2 using Malaria4, excel append ctitle(`var') stats(coef se) nocons depvar label dec(5)
	
	reg `var' i.groups $X
	outreg2 using Malaria4, excel append ctitle(`var') stats(coef se) nocons depvar label dec(5)
}


use psid_controls_new.dta, clear

* Obtener la diferencia de medias incondicional (y la desviación estándar de la diferencia de medias) entre los tratados y los no tratados.

ttest re78, by(treat) reverse
local diff=`r(mu_1)'-`r(mu_2)' /*guardar en un local la diferencia de medias */

mat A=[`diff' \ `r(se)'] /*poner en una matriz la diferencia de medias y su desviación estándar */
mat list A

mat drop A

* Calcular la diferencia de medias entre tratados y no tratados con el método de propensity score matching.

psmatch2 treat age education married, outcome(re78) 
matrix A = ( `r(att)' \ `r(seatt)' ) /*guardar en una matriz el estimador de PSM y su desviación estandar */
mat list A
psmatch2 treat age education married black hispanic nodegree re74 re75, outcome(re78) 
matrix B = ( `r(att)' \ `r(seatt)' ) /*guardar en una matriz el estimador de PSM y su desviación estandar */
mat list B

mat C = [A, B] /*hacer una matriz con los estimadores de PSM con las dos especificaciones */
mat list C
