/*
Author: 		Joe Long, Innovations for Poverty Action
Date Created: 	October 16, 2013
Last Edited:	October 16, 2013
*/
cap program drop orth_out
program orth_out
	version 12
	syntax varlist using [if], BY(varlist) [LAbel] 
	
	marksample touse
	tempvar treatment_type
	
	loc ntreat: word count `by'
	
	if `ntreat' == 1{
		levelsof `by' 
		loc arms `r(levels)'
		di `arms'
		loc ntreat: word count `arms'
		loc n 0 
		foreach val of loc arms{
			loc ++n
			tempvar treatarm`n'
			gen `treatarm`n'' = 1 if `by' == `val'
		}
		loc by ""
		forvalues m = 1/`ntreat'{
			loc by "`by' `treatarm`m''"	
		}
	}
	
	loc varcount: word count `varlist'
	loc by2 `by'
	loc m = (`ntreat'^2+`ntreat')/2
	egen `treatment_type' = group(`by')
	tab `treatment_type'
	mat A = J(2*`varcount'+1, `m'+1, .)
	loc r 0
	foreach var in `varlist'{
		loc ++r
		
		tabstat `var' , by(`treatment_type') stats(mean semean) save
		forvalues n = 1/`ntreat'{
			mat A[`r',`n'] = r(Stat`n')
			}
		mat list A
		
		loc j = `ntreat' 
		
		forvalues n = 1/`ntreat'{
			gettoken var1 by: by
			foreach var2 of loc by{
				reg `var' `var1' if (`var1'==1 | `var2'==1) 
				loc ++j
				loc b = _b[`var1']
				loc se = _se[`var1']
				mat A[`r',`j'] = `b'
				mat A[`r'+1,`j'] = `se'
			}
		}
		loc by `by2'
		reg `var' `by2' 
		mat A[`r', `m'+1] = Ftail(e(df_m), e(df_r), e(F))
		
		loc ++r	
	}
	tempvar count
	gen `count' = 1
	tabstat `count' , by(`treatment_type') stats(n) save
		forvalues n = 1/`ntreat'{
			mat A[2*`varcount'+1,`n'] = r(Stat`n')
		}
			
	if "`label'" == "label"{
		loc rnames ""
		foreach var of loc varlist{
			loc rname: var la `var'
			if "`rname'" == ""{
				loc rname `var'
			}
			loc rnames "`rnames' "`rname'" " ""
		}
		loc rnames "`rnames' "N""
		
		loc cnames ""
		foreach var of loc by{
			loc cname: var la `var'
			if "`cname'" == ""{
				loc cname "`var'"
			}
			loc cnames ""`cname'" `cnames'"
		}
		loc num ""
		forvalues n = 1/`ntreat'{
			loc num "`num' `n'"
		}
		loc cnames2 ""
		forvalues n = 1/`ntreat'{
		gettoken num1 num: num 
			foreach num2 of loc num{
				loc cnames2 "`cnames2' "(`num1') vs. (`num2')""
			}
		}
		loc cnames "`cnames' `cnames2' "p-value""

	}
	else {
		loc rnames ""
		loc cnames ""
		}
		di `"`rnames'"'
		di `"`cnames'"'
	xml_tab A, rnames(`rnames') cnames(`cnames') save(`using') font("Times New Roman" 10) replace title(Orthogonality Tests) line(COL_NAMES 13 LAST_ROW 13) format( NCCR3)
	mat drop A	
end
