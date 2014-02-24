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
forvalues n = 1/3{
	replace group = `n' if _n > `=`n'-1'/4*_N
}
qui gen var1 = 10*runiform()
qui gen var2 = runiform()
la var var1 "Variable 1"
la de group 1 "A" 2 "B" 3 "C" 
la val group group

*Calculate correct values for matrix
*HORIZONTAL
mat A = J(2, 10, .)
forvalues n = 1/3{
	qui tabstat var1 if group == `n', s(mean semean) save
	mat A[1,`n'] = r(StatTotal)
}
qui tabstat var1, s(mean semean) save
mat A[1, 4] = r(StatTotal)
forvalues n = 1/3{
	gen g`n' = group == `n' 
}
reg var1 g1 if inlist(group, 1, 2)
mat A[1, 5] = _b[g1]
mat A[2, 5] = _se[g1]
reg var1 g1 if inlist(group, 1, 3)
mat A[1, 6] = _b[g1]
mat A[2, 6] = _se[g1]
reg var1 g2 if inlist(group, 2, 3)
mat A[1, 7] = _b[g2]
mat A[2, 7] = _se[g2]
reg g1 var1
mat A[1, 8] = _b[var1]
mat A[2, 8] = _se[var1]
reg var1 i.group 
mat A[1, 9] = Ftail(e(df_m), e(df_r), e(F))
mat A[1, 10] = e(N)
noi mat li A, format(%12.5f)
*VERTICAL
mat B = J(4, 3, .)
forvalues n = 1/3{
	qui tabstat var1 if group == `n', s(mean semean n) save
	mat B[1,`n'] = r(StatTotal)
}
loc total = B[3,1] + B[3,2] + B[3,3]
forval n = 1/3 {
	mat B[4,`n'] = B[3,`n']/`total'
}
noi mat li B, format(%12.5f)

*Compare displayed matrix to calculated one
#d ;
loc options
	compare 
	overall
	prop
	test
	vcount
	r
	;
#d cr

*HORIZONTAL
orth_out var1, by(group) bdec(7) se compare overall r test vcount
assert mreldif(A , r(matrix)) < 1E-5

*VERTICAL
orth_out var1, by(group) bdec(7) se count prop
assert mreldif(B , r(matrix)) < 1E-5

*Compare exported matrix to calculated one
tempfile test
*HORIZONTAL
preserve
orth_out var1 using `test', by(group) bdec(7) se compare overall r test vcount
	import excel `test', first all clear
	drop OrthogonalityTable
	drop if _n == 1
	foreach var of varlist _all{
		qui replace `var' = subinstr(`var', "(", "",.)
		qui replace `var' = subinstr(`var', ")", "",.)
		*replace `var' = subinstr(`var', "*", "",.)
	}
	qui destring _all, replace
	mkmat _all, mat(C)
	noi mat li C
	assert mreldif(A , C) < 1E-5
restore

*VERTICAL
preserve
orth_out var1 using `test', by(group) bdec(7) se count prop replace
	import excel `test', first all clear
	drop OrthogonalityTable
	drop if _n == 1
	foreach var of varlist _all{
		qui replace `var' = subinstr(`var', "(", "",.)
		qui replace `var' = subinstr(`var', ")", "",.)
		*replace `var' = subinstr(`var', "*", "",.)
	}
	qui destring _all, replace
	mkmat _all, mat(D)
	noi mat li D
	assert mreldif(B , D) < 1E-5
restore
