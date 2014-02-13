/*
Author: 		Joe Long, Innovations for Poverty Action
Date Created: 	October 3, 2013
Last Edited:	October 3, 2013
Function: 		This do-file creates an orthogonality table, with columns for all treatment
				arms as well as ones for comparisons between every treatment arm. 
Notes:			Local the list of variables that you are performing the orthogonality check on under "var". 
				Local the list of treatment arms that you are checking orthogonality on under "treatarms".
				Local the dataset under "dataset". 
				This do-file is adapted from do-file by Ellen Degnan.
*/

set more off
set varabbrev on 

#d ; 
	loc vars
		q10_female
		married
		q12_hhhead
		q14_age
		q15_edu
		q16_fedu
		q22a_ownincome
		q22b_hhincome
		audit
		unreach_wk3_or_wk4
		wk3_seeflier_0
		wk4_seeflier_0
		delivered
		ask
		;
	loc treatarms
		treat_p_p 
		treat_i_p 
		treat_i_i
		control
		;
	loc dataset
		balance_check_trim.dta
		;
#d cr

loc ntreat: word count `treatarms'
loc varcount: word count `vars'
loc treatarms2 `treatarms'
loc m = (`ntreat'^2+`ntreat')/2

cap log close
log using `dataset'.log, replace

use `dataset', clear

cap drop treatment_type
gen treatgroup = 0
loc n 0

foreach var of varlist `treatarms'{
	loc ++n
	replace treatment_type = `n' if `var' == 1
}

mat A = J(2*`varcount'+1, `m'+1, .)
loc r 0
foreach var in `vars'{
	loc ++r
	
	tabstat `var', by(treatment_type) stats(mean semean) save
	forvalues n = 1/`ntreat'{
		mat A[`r',`n'] = r(Stat`n')
		}
	mat list A
	
	loc j = `ntreat' 
	
	forvalues n = 1/`ntreat'{
		gettoken var1 treatarms: treatarms
		foreach var2 of loc treatarms{
			reg `var' `var1' if `var1'==1 | `var2'==1
			loc ++j
			loc b = _b[`var1']
			loc se = _se[`var1']
			mat A[`r',`j'] = `b'
			mat A[`r'+1,`j'] = `se'
		}
	}
	loc treatarms `treatarms2'
	reg `var' `treatarms2'
	mat A[`r', `m'+1] = Ftail(e(df_m),e(df_r),e(F))
	
	loc ++r	
}
gen count = 1
tabstat count, by(treatment_type) stats(n) save
	forvalues n = 1/`ntreat'{
		mat A[2*`varcount'+1,`n'] = r(Stat`n')
	}


xml_tab A, save(test_orthogonality.xml) replace title(Appendix A: Orthogonality Tests: Respondent Characteristics at Baseline; As Reported at Endline) line(COL_NAMES 13 LAST_ROW 13) format( NCCR3)
mat drop A
