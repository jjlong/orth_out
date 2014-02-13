/*
Author: 		Joe Long, Innovations for Poverty Action
Date Created: 	October 16, 2013
Last Edited:	October 21, 2013
*/
cap program drop orth_out
program orth_out
	version 12
	syntax varlist using/ [if], BY(varlist) [replace] [append] [SHEETname(string)] [NOGrid] [BDec(numlist)] [COMPare] [count]  [NOLAbel] [ARMLAbel(string)] [VARLAbel(string asis)] [NUMLAbel] [COLNUM] [Title(string)] [NOTEs(string)] 
	qui{
		preserve 
		if "`if'"!=""{
			keep if `if'
		}
		loc byrep ""
		loc ntreat: word count `by'
		forvalues n = 1/`ntreat'{
			loc word: word `n' of `by' 
			loc byrep "`word' `byrep'"
		}
		loc by `byrep'
		if `ntreat' > 1{
			tempvar treatment_type
			egen `treatment_type' = group(`by')
		}
		else if `ntreat' == 1{
			levelsof `by' 
			loc arms `r(levels)'
			loc n 0
			loc cnames ""
			foreach val of loc arms{
				loc ++n
				loc cname: lab `by' `val' 
				if "`cname'" == ""{
					loc cname "`n'"
				}
				loc cnames "`cnames' "`cname'""
			}
			loc ntreat: word count `arms'
			loc n 0 
			foreach val of loc arms{
				loc ++n
				tempvar treatarm`n'
				gen `treatarm`n'' = `by' == `val'
			}
			tempvar treatment_type 
			gen `treatment_type' = `by'
			loc by ""
			forvalues m = 1/`ntreat'{
				loc by "`by' `treatarm`m''"	
			}
		}	
		loc varcount: word count `varlist'
		loc by2 `by'
		if "`compare'" == "compare"{
			loc m = (`ntreat'^2+`ntreat')/2
		}
		else{
			loc m = `ntreat'
		}	
		
		if "`count'" == "count" {
			loc count = 1
		}
		else {
			loc count = 0
		}
		tempname A
		mat `A' = J(2*`varcount'+`count', `m'+1, .)
		loc r 0
		foreach var in `varlist'{
			loc ++r
			
			tabstat `var' , by(`treatment_type') stats(mean semean) save
			forvalues n = 1/`ntreat'{
				mat `A'[`r',`n'] = r(Stat`n')
				}
			mat list `A'
			
			loc j = `ntreat' 
			if "`compare'" == "compare"{
				forvalues n = 1/`ntreat'{
					gettoken var1 by: by
					foreach var2 of loc by{
						reg `var' `var1' if (`var1'==1 | `var2'==1) 
						loc ++j
						loc b = _b[`var1']
						loc se = _se[`var1']
						mat `A'[`r',`j'] = `b'
						mat `A'[`r'+1,`j'] = `se'
					}
				}
			}
			loc by `by2'
			reg `var' `by2' 
			mat `A'[`r', `m'+1] = Ftail(e(df_m), e(df_r), e(F))
			
			loc ++r	
		}
		if `count'{
			tempvar count
			gen `count' = 1
			tabstat `count' , by(`treatment_type') stats(n) save
			forvalues n = 1/`ntreat'{
				mat `A'[2*`varcount'+1,`n'] = r(Stat`n')
			}
		}

		if "`nolabel'" != "nolabel"{
			loc rnames ""
			if `"`varlabel'"' != ""{
				loc varlist2 `varlist'
				forvalues n = 1/`varcount' {
					gettoken var varlist: varlist
					loc lab`n': word `n' of `varlabel'
					la var `var' "`lab`n''"
				}
			}
			foreach var of loc varlist{
				loc rname: var la `var'
				if "`rname'" == ""{
					loc rname `var'
				}
				loc rnames "`rnames' "`rname'" " ""
			}
			if `count' {
				loc rnames "`rnames' "N""
			}
			if "`armlabel'"!=""{
				loc cnames ""
				loc ccount: word count `armlabel'
					if `ccount' == `ntreat'{
						loc cnames `armlabel'
					}
			}
			else if "`numlabel'" == "numlabel"{
				loc cnames ""
				forvalues n = 1/`ntreat'{
					loc cnames "`cnames' (`n')"
				}
			}
			else if "`arms'" == ""{
				loc cnames ""
				foreach var of loc by{
					loc cname: var lab `var'
					if "`cname'" == ""{
						loc cname "`var'"
					}
					loc cnames ""`cname'" `cnames'"
				}
			}
			loc num ""
			forvalues n = 1/`ntreat'{
				loc num "`num' `n'"
			}
			if "`compare'" == "compare"{
				loc cnames2 ""
				forvalues n = 1/`ntreat'{
				gettoken num1 num: num 
					foreach num2 of loc num{
						loc cnames2 "`cnames2' "(`num1') vs. (`num2')""
					}
				}
			}
			loc cnames "`cnames' `cnames2' "p-value from joint orthogonality test of all treatment arms""
		}
		else {
			loc rnames ""
			loc cnames ""
			}
		if "`colnum'" == "colnum"{
			loc column ""
			forvalues n = 0/`m'{
				loc p = `n' + 1
				loc column "`column' "(`p')""
			}
		}
		if "`bdec'"==""{
			loc bdec = 3
		}
		loc format "SCCR0"
		loc labformat "SCLR0"
		loc rownum = 2*`varcount'
		forvalues n = 1/`rownum'{
			loc labformat "`labformat' SCLR0" 
			loc format "`format' NCCR`bdec'"
		}
		loc format "`format' SCCR0"
		if "`title'" == ""{
			loc title "Orthogonality Table"
		}
		if "`nogrid'" == "nogrid"{
			loc nogrid ", nogridlines"
		}
		set trace off
		noi xml_tab `A', rnames(`rnames') cnames(`column') ceq(`cnames') showeq sheet(`sheetname' `nogrid')  save("`using'") font("Times New Roman" 10) `replace' `append' title(`title') notes(`notes') line(COL_NAMES 13 LAST_ROW 13) format((`labformat') (`format'))
		mat drop `A'	
		restore
	}
end
