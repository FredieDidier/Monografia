* regressao: job loss

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
	*/	addtext(State FE, Yes, Urban FE, Yes) /*
	*/	tex(fragment) /*
	*/	

* Regressão 3
reghdfe job_loss ///
educ2 educ3 educ4 /// 
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
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
	*/	addtext(State FE, Yes, Urban FE, Yes, Time-varying observables, Yes) /*
	*/	tex(fragment) /*
	*/
	
* Regressão 4	
reghdfe job_loss ///
educ2 educ3 educ4 /// 
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
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
	*/	addtext(State FE, Yes, Urban FE, Yes, Time-varying observables, Yes, Occupation and Sector FE, Yes) /*
	*/	tex(fragment) /*
	*/
	
* Regressão 5
reghdfe job_loss ///
educ2 educ3 educ4 ///  
 signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
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
	*/	addtext(State FE, Yes, Urban FE, Yes, Ind FE, Yes, Time-varying observables, Yes, Occupation and Sector FE, Yes) /*
	*/	tex(fragment) /*
	*/	
