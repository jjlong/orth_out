/*
Author: 		Joe Long, Innovations for Poverty Action
Date Created: 	February 24, 2014
Date Edited: 	February 24, 2014
Notes:			This do-file tests orth_out.ado
*/

cscript orth_out adofile orth_out

*Generate dataset
clear
set obs 100000
set seed 12894702
gen group = .
forvalues n = 1/4{
	replace group = `n' if _n > `=`n'-1'/4*_N
}
gen var1 = _n/_N*10
gen var2 = 0
la var var1 "Variable 1"
la de group 1 "A" 2 "B" 3 "C" 4 "D"
la val group group

*Calculate correct values for matrix
mat A = J(2, 4, .)
forvalues n = 1/4{
	tabstat var1 if group == `n', s(mean semean) save
	mat A[1,`n'] = r(StatTotal)
}
mat li A

*Compare displayed matrix to calculated one

*Compare exported matrix to calculated one
