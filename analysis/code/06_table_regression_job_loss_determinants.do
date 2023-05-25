
* regressao: job loss

global CONTROLS signed_work_card cnpj job_function hours_worked temporary_worker position social_security_taxpayer gender race age monthly_work_income weights job_start educ urbana state

* edit indpendent variables
cap drop educ1
gen educ1 = 1 if educ ==1 
replace educ1 = 0 if educ1 ==.
label variable educ1 "Incomplete primary school"

cap drop educ2
gen educ2 = 1 if educ ==2
replace educ2 = 0 if educ2 ==.
label variable educ2 "Incomplete high school"

cap drop educ3
gen educ3 = 1 if educ ==3
replace educ3 = 0 if educ3 ==.
label variable educ3 "Incomplete college"

cap drop educ4
gen educ4 = 1 if educ ==4
replace educ4 = 0 if educ4 ==.
label variable educ4 "Complete college"


* é preciso instalar o pacote "reghdfe"

*******************************************************************************
* install packages for replication
*******************************************************************************

* net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/") // for reghdfe

* Regressão 1
reghdfe job_loss ///
educ2 educ3 educ4 /// 
[aw=weights] ///
, noabsorb

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext() /*
	*/	tex(fragment) /*
	*/	replace

* Regressão 2
reghdfe job_loss ///
educ2 educ3 educ4 /// 
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, noabsorb

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(State FE, Yes, City FE, Yes) /*
	*/	tex(fragment) /*
	*/	

* Regressão 3
reghdfe job_loss ///
educ2 educ3 educ4 /// 
signed_work_card cnpj job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, noabsorb

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(State FE, Yes, City FE, Yes) /*
	*/	tex(fragment) /*
	*/
	
* Regressão 4	
	
	
* Regressão 5
reghdfe job_loss ///
educ2 educ3 educ4 ///  
 job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
,  absorb(ind )

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(State FE, Yes, City FE, Yes, Ind FE, Yes) /*
	*/	tex(fragment) /*
	*/	
