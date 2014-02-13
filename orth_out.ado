*! version 2.4.0 Joe Long 20dec2013
cap program drop orth_out
program orth_out, rclass
	version 12
	syntax varlist [using] [if], BY(varlist) [replace] ///
		[SHEET(string) SHEETREPlace BDec(numlist) COMPare count vcount]  ///
		[NOLAbel ARMLAbel(string) VARLAbel(string asis) NUMLAbel] ///
		[COLNUM Title(string) NOTEs(string) test overall] ///
		[PROPortion SEmean COVARiates(varlist)] ///
		[INTERACTion Reverse reverseall append stars]	
		
	preserve
	if `"`if'"' != ""{
		qui keep `if'
	}
	loc ntreat: word count `by'
	forvalues n = 1/`ntreat' {
		loc word: word `n' of `by'
		loc byrep "`word' `byrep'"
	}
	loc by `byrep'
	if `ntreat' > 1 {
		tempvar treatment_type
		egen `treatment_type' = group(`by')
	}
	else {
		qui levelsof `by', local(arms)
		loc n 0
		loc vallab: val lab `by'
		foreach val of loc arms {
			loc ++n
			if "`vallab'" != ""{
				loc cname: lab `vallab' `val'
			}
			else{
				loc cname: lab `by' `val'
			}
			loc cnames "`cnames' "`cname'""
		}

		loc ntreat : word count `arms'
		
		loc n 0
		foreach val of loc arms {
			loc ++n
			tempvar treatarm`n'
			gen `treatarm`n'' = `by' == `val'
		}

		tempvar treatment_type
		gen `:type `by'' `treatment_type' = `by'
		loc by ""
		forvalues m = 1/`ntreat'{
			loc by "`by' `treatarm`m''"
		}
	}

	loc varcount: word count `varlist'
	loc by2 `by'
	if "`compare'" != ""{
		loc m = (`ntreat'^2+`ntreat')/2
	}
	else{
		loc m = `ntreat'
	}
	
	if "`interaction'" != ""{
		loc interaction 
		foreach var1 of local covariates{
			foreach var2 of local by{
				tempvar `var1'X`var2'
				gen ``var1'X`var2'' = `var1' * `var2'
				loc interaction `interaction' ``var1'X`var2''
			}
		}
	}
	
	loc count 	= 1 - mi("`count'")
	loc ftest 	= 1 - mi("`test'")
	loc overall	= 1 - mi("`overall'")
	loc prop    = 1 - mi("`proportion'")
	loc sterr	= 2 - mi("`semean'")
	loc interact= 1 - mi("`interaction'")
	loc reverse = 1 - mi("`reverse'")
	loc reverseall = 1 - mi("`reverseall'")
	loc vcount = 1 - mi("`vcount'")
	
	tempname A
	mat `A' = J(`sterr'*`varcount'+`count'+`prop', `m'+`reverse'+`reverseall'+`overall'+`test'+`vcount', .)
	loc r 0
	foreach var in `varlist' {
		loc ++r

		qui tabstat `var' , by(`treatment_type') stats(mean `semean') save
		forvalues n = 1/`ntreat'{
			mat `A'[`r',`n'] = r(Stat`n')
		}
		if `overall'{
			mat `A'[`r', `ntreat'+1] = r(StatTotal)
		}
		loc j = `ntreat' + `overall'
		if "`compare'" != ""{
			forvalues n = 1/`ntreat'{
				gettoken var1 by: by
				foreach var2 of loc by{
					qui reg `var' `var1' if (`var1'==1 | `var2'==1)
					loc ++j
					loc b = _b[`var1']
					loc se = _se[`var1']
					mat `A'[`r',`j'] = `b'
					if "`semean'" != ""{
						mat `A'[`r'+1,`j'] = `se'
						if "`stars'" != ""{
							if abs(`b'/`se') >= invnorm(0.995) {
								loc star_`=`j'-`ntreat'-`overall'' "`star_`=`j'-`ntreat'-`overall'''" "***"
							}
							else if abs(`b'/`se') >= invnorm(0.975){
								loc star_`=`j'-`ntreat'-`overall'' "`star_`=`j'-`ntreat'-`overall'''" "**"							
							}
							else if abs(`b'/`se') >= invnorm(0.95){
								loc star_`=`j'-`ntreat'-`overall'' "`star_`=`j'-`ntreat'-`overall'''" "*"							
							}
							else {
								loc star_`=`j'-`ntreat'-`overall'' "`star_`=`j'-`ntreat'-`overall'''" " "
							}
						}
					}
				}
			}
		}
		loc by `by2'
		if `reverse'{
			qui reg `:word 1 of `by'' `var' `covariates' `interaction', noheader
			mat `A'[`r', `m'+`overall'+`reverse'] = _b[`var']
			if `sterr' == 2{
				mat `A'[`r'+1, `m'+`overall'+`reverse'] = _se[`var']
				loc b _b[`var']
				loc se _se[`var']
				if "`stars'" != ""{
					if abs(`b'/`se') >= invnorm(0.995) {
						loc star_`=`m'+`overall'+`reverse'' "`star_`=`m'+`overall'+`reverse'''" "***"
					}
					else if abs(`b'/`se') >= invnorm(0.975){
						loc star_`=`m'+`overall'+`reverse'' "`star_`=`m'+`overall'+`reverse'''" "**"							
					}
					else if abs(`b'/`se') >= invnorm(0.95){
						loc star_`=`m'+`overall'+`reverse'' "`star_`=`m'+`overall'+`reverse'''" "*"							
					}
					else {
						loc star_`=`m'+`overall'+`reverse'' "`star_`=`m'+`overall'+`reverse'''" " "
					}
				}
			}
		}
		if `test'|`vcount'{
			qui reg `var' `by' `covariates' `interaction', noheader 
			if `test'{
				mat `A'[`r', `m'+`overall'+`reverse'+`reverseall'+`test'] = Ftail(e(df_m), e(df_r), e(F))
			}
			if `vcount'{
				mat `A'[`r', `m'+`overall'+`reverse'+`reverseall'+`test'+`vcount'] = e(N)
			}
		}
		loc r = `r' + (`sterr' - 1)
	}
	if `reverseall'{
		loc r 0
		qui reg `:word 1 of `by'' `varlist' `covariates' `interaction', noheader
		foreach var of local varlist{
			loc ++r
			mat `A'[`r', `m'+`overall'+`reverse'+`reverseall'] = _b[`var']
			if `sterr' == 2{
				loc ++r
				mat `A'[`r', `m'+`overall'+`reverse'+`reverseall'] = _se[`var']
				loc b _b[`var']
				loc se _se[`var']
				if "`stars'" != ""{
					if abs(`b'/`se') >= invnorm(0.995) {
						loc star_`=`m'+`overall'+`reverse'+`reverseall'' "`star_`=`m'+`overall'+`reverse'+`reverseall'''" "***"
					}
					else if abs(`b'/`se') >= invnorm(0.975){
						loc star_`=`m'+`overall'+`reverse'+`reverseall'' "`star_`=`m'+`overall'+`reverse'+`reverseall'''" "**"							
					}
					else if abs(`b'/`se') >= invnorm(0.95){
						loc star_`=`m'+`overall'+`reverse'+`reverseall'' "`star_`=`m'+`overall'+`reverse'+`reverseall'''" "*"							
					}
					else {
						loc star_`=`m'+`overall'+`reverse'+`reverseall'' "`star_`=`m'+`overall'+`reverse'+`reverseall'''" " "
					}
				}
			}
		}
	}
	if `count' | `prop' {
		tempvar N
		gen `N' = 1
		qui tabstat `N', by(`treatment_type') stats(n) save
		forvalues n = 1/`ntreat'{
			if `count' {
				mat `A'[`sterr'*`varcount'+`count',`n'] = r(Stat`n')
			}
			if `prop' {
				mat `A'[`sterr'*`varcount'+`count'+`prop',`n'] = r(StatTotal)
				mat `A'[`sterr'*`varcount'+`count'+`prop',`n'] = `A'[`sterr'*`varcount'+`count',`n']/`A'[`sterr'*`varcount'+`count'+`prop',`n']
			}
		}
		if `overall'{
			mat `A'[`sterr'*`varcount'+`count',`ntreat'+1] = r(StatTotal)
			if `prop'{					
				mat `A'[`sterr'*`varcount'+`count'+`prop',`ntreat'+1] = 1
			}		
		}
	}
	if "`nolabel'" == "" {
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
			if "`semean'"!=""{
				loc rnames "`rnames' "`rname'" " ""
			}
			else {
				loc rnames "`rnames' "`rname'""
			}
		}
		if `count' {
			loc rnames "`rnames' "N""
		}
		if `prop' {
			loc rnames "`rnames' "Proportion""
		}
		if "`armlabel'"!=""{
			loc ccount: word count `armlabel'
				if `ccount' == `ntreat'{
					loc cnames `armlabel'
				}
		}
		else if "`numlabel'" != ""{
			forvalues n = 1/`ntreat'{
				loc cnames "`cnames' (`n')"
			}
		}
		else if "`arms'" == ""{
			foreach var of loc by{
				loc cname: var lab `var'
				if "`cname'" == ""{
					loc cname "`var'"
				}
				loc cnames ""`cname'" `cnames'"
			}
		}
		forvalues n = 1/`ntreat'{
			loc num "`num' `n'"
		}
		if `overall'{
			loc cnames "`cnames' "Overall""
		}
		if "`compare'" != ""{
			forvalues n = 1/`ntreat'{
			gettoken num1 num: num
				foreach num2 of loc num{
					loc cnames2 "`cnames2' "(`num1') vs. (`num2')""
				}
			}
		}
		loc cnames "`cnames' `cnames2'"
		if `reverse'{
			if `sterr' == 2 {
				loc standard "s. & s.e."
			}
			else {
				loc standard "icients"
			}
			loc cnames "`cnames' "Coeff`standard', treatment as dep. variable""
		}
		if `reverseall'{
			if `sterr' == 2 {
				loc standard "s. & s.e."
			}
			else {
				loc standard "icients"
			}
			loc cnames "`cnames' "Coeff`standard', treatment as dep. variable, all balance variables together""
		}
		if `test'{
			loc cnames "`cnames' "p-value from joint orthogonality test of treatment arms""
		}
		if `vcount'{
			loc cnames "`cnames' "N from orthogonality test""
		}
	}
	else {
		loc rnames ""
		loc cnames ""
	}
	if "`colnum'" != "" {
		loc column ""
		loc p = `m'+`reverse'+`overall'+`reverseall'+`test'+`vcount'
		forvalues n = 1/`p'{
			loc column "`column' "(`n')""
		}
	}
	if "`bdec'"==""{
		loc bdec = 3
	}
	
	if "`title'" == ""{
		loc title "Orthogonality Table"
	}
	forvalues n = 1/`varcount'{
		loc req "`req' mean"
		if `sterr' == 2{
			loc req "`req' se"
		}
	}
	if `count'{
		loc req "`req' _"
	}
	if `prop'{
		loc req "`req' _"
	}
	if "`using'" != ""{
		clear
		qui svmat `A'
		tempvar n
		qui tostring _all, replace force format(%12.`bdec'f)
		qui gen `n' = _n + 2
		tempvar B0
		qui gen `B0' = ""
		if `sterr' == 2{
			foreach var of varlist `A'*{
				qui replace `var' = "(" + `var' + ")" if `var' != "." & mod(`n', 2) == 0 
			}
			if "`compare'" != "" & "`stars'" != ""{
				qui su `n'
				forvalues j = 1/`=(`ntreat'^2-`ntreat')/2'{
					forvalues p = `r(min)'/`r(max)'{
						if mod(`p', 2) == 0{
							qui replace `A'`=`j'+`ntreat'+`overall'' = `A'`=`j'+`ntreat'+`overall'' + "`:word `=`p'/2' of "`star_`j''"'" ///
								if substr(`A'`=`j'+`ntreat'+`overall'', 1, 1) == "(" & `n' == `p'
						}
					}
				}
			}
			if `reverse' & "`stars'" != ""{
				qui su `n'
				forvalues p = `r(min)'/`r(max)'{
					if mod(`p', 2) == 0{
						qui replace `A'`=`m'+`overall'+`reverse'' = `A'`=`m'+`overall'+`reverse'' + "`:word `=`p'/2' of "`star_`=`m'+`overall'+`reverse'''"'" ///
							if substr(`A'`=`m'+`overall'+`reverse'', 1, 1) == "(" & `n' == `p'
					}
				}
			}
			if `reverseall' & "`stars'" != ""{
				qui su `n'
				forvalues p = `r(min)'/`r(max)'{
					if mod(`p', 2) == 0{
						qui replace `A'`=`m'+`overall'+`reverse'+`reverseall'' = `A'`=`m'+`overall'+`reverse'+`reverseall'' + "`:word `=`p'/2' of "`star_`=`m'+`overall'+`reverse'+`reverseall'''"'" ///
							if substr(`A'`=`m'+`overall'+`reverse'+`reverseall'', 1, 1) == "(" & `n' == `p'
					}
				}
			}
		}
		if `vcount'{
			qui replace `A'`=`m'+`overall'+`reverse'+`reverseall'+`test'+`vcount'' = substr(`A'`=`m'+`overall'+`reverse'+`reverseall'+`test'+`vcount'', 1, length(`A'`=`m'+`overall'+`reverse'+`reverseall'+`test'+`vcount'')-4)
		}
		loc p = 2
		foreach name in `rnames'{
			loc ++p
			qui replace `B0' = "`name'" if `n' == `p' & "`name'" != "_"
		}
		qui d, s
		loc N = `r(N)' + 1
		qui set obs `N'
		qui replace `n' = 1 if `n' == .
		sort `n'

		forvalues m = 1/`:word count `cnames''{
			qui replace `A'`m' = "`:word `m' of `cnames''" if `n' == 1
		}
		if "`colnum'" != ""{	
			loc N = `N' + 1
			qui set obs `N'
			qui replace `n' = 2 if `n' == .
			sort `n'
			forvalues m = 1/`:word count `column''{
				qui replace `A'`m' = "`:word `m' of `column''" if `n' == 2
			}
		}
		if "`title'" != ""{
			loc N = `N' + 1
			qui set obs `N' 
			qui replace `n' = 0 if `n' == . 
			sort `n' 
			qui replace `B0' = "`title'" if `n' == 0
		}
		if "`notes'" != ""{
			loc N = `N' + 1
			qui set obs `N' 
			sort `n' 
			replace `B0' = "`notes'" if mi(`n')
		}
		loc note = 1 - mi("`notes'")
		foreach var of varlist `A'*{
			if `count'{
				loc normal = `bdec' != 0
				qui replace `var' = substr(`var', 1, length(`var')-`bdec'-`normal') if `B0' == "N" & "`var'" != "`B0'"
			}
			if `prop'{
				qui replace `var' = substr(`var', 2, length(`var')-2) if `B0' == "Proportion" & "`var'" != "`B0'"					
			}
		}
		qui ds, has(type string)
		foreach var of varlist `r(varlist)'{
			qui replace `var' = "" if `var' == "."
		}
		order `B0', first
		drop `n'
		if "`append'" != ""{
			qui ds
			if `:word count `r(varlist)'' > 26{
				di as err "yo gurrrl u has 2 many treatments. pls re-evaluate yo lyfe decisions."
				err 197
			}
			forvalues q = 1/`:word count `r(varlist)''{
				rename `:word `q' of `r(varlist)'' `:word `q' of `c(ALPHA)''
			}
			tempfile temp
			qui save `temp'
			import excel `using', clear
			append using `temp'
			noi di "table appended to `:word 2 of `using''"
			loc replace replace
		}
		noi export excel _all `using', `replace' sheet("`sheet'") `sheetmodify' `sheetreplace'
	}
	if `"`column'"' == ""{
		forvalues n = 1/`=`m'+`reverse'+`overall'+`test''{
			loc column "`column' _"
		}
	}
	mat rown   `A' = `req'	
	mat coln   `A' = `column'
	mat roweq  `A' = `rnames'
	mat coleq  `A' = `cnames'
	noi mat li `A', noheader format(%12.`bdec'f)
	
	return loc rnames `rnames' 
	return loc cnames `cnames'
	return loc title  `title'
	return matrix matrix `A'
end
