cap file close handle
file open handle using handle.txt, write replace
file w handle "{" _n
file w handle "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
file w handle "\begin{tabular*}{\hsize}{@{\hskip\tabcolsep\extracolsep\fill}l*{2}{cc}}" _n
file w handle "\hline\hline" _n
sysuse auto, clear
orth_out price mpg, by(foreign) se count compare
mat A = r(matrix)
forvalues m = 1/`=colsof(A)' {
	file w handle "&\multicolumn{1}{c}{(`m')}"
}
file w handle "\\" _n
forvalues m = 1/`=colsof(A)' {
	file w handle "&\multicolumn{1}{c}{`:word `m' of "`r(cnames)'"'}"
}
file w handle "\\" _n "\hline" _n

forvalues n = 1/`=rowsof(A)' {
	loc row`n' ""
	forvalues m = 1/`=colsof(A)' {
		if "`:word `n' of "`r(rnames)'"'" != " "{
			if "`:word `n' of "`r(rnames)'"'" != "N" {
				loc row`n' "`row`n'' & `=string(A[`n', `m'], "%9.3f")'"
			}
			else {
				loc row`n' "`row`n'' & `=string(A[`n', `m'], "%9.0f")'"
			}
		}
		else {
			if "`stars'" == "" {
				loc row`n' "`row`n'' & (`=string(A[`n', `m'], "%9.3f")')"
			}
			else {
				loc row`n' "`row`n'' & (`=string(A[`n', `m'], "%9.3f")')\sym(***)"
			}
		}
	}
	if "`:word `n' of "`r(rnames)'"'" != "N" {
		file write handle "`:word `n' of "`r(rnames)'"' `row`n'' \\" 
	}
	else {
		file write handle "\hline" _n "\(N\) `row`n'' \\" 
	}
}
file w handle _n "\hline" _n "\end{tabular*}"
file close handle
