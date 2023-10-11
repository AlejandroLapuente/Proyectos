
cd "/Users/alejandrolapuente/Desktop/Alejandro/CIDE/7mo semestre/Microeconometrics/Repaso Examen"

net from http://www.stata-press.com/data/musr
net install musr
net get musr

use mus06data, clear

sum totchr age female blhisp income
sum drugexp income, de

**-----2------**

eststo clear

reg ldrugexp hi_empunion
eststo reg1

reg ldrugexp hi_empunion totchr
eststo reg2

reg ldrugexp hi_empunion totchr female blhisp
eststo reg3

esttab

outreg2 [reg1 reg2 reg3] using Prueba, replace word


**-----3------**

global X totchr female blhisp linc age age2

psmatch2 hi_empunion $X, outcome(ldrugexp) ate
