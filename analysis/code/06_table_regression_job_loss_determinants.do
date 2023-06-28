* regressao: job loss

* Regressão 1
reg job_loss ///
educ1 educ2 educ3 educ4 /// 
[aw=weights] ///
, nocons

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

test educ1 = educ2
global fstat_2 : di %9.3fc `r(F)' 
global pvalue_2 : di %9.3fc `r(p)' 
di "F statistic = $fstat_2"
di "Pvalue = $pvalue_2"

test educ1 = educ3
global fstat_3 : di %9.3fc `r(F)' 
global pvalue_3 : di %9.3fc `r(p)' 
di "F statistic = $fstat_3"
di "Pvalue = $pvalue_3"

test educ1 = educ4
global fstat_4 : di %9.3fc `r(F)' 
global pvalue_4 : di %9.3fc `r(p)' 
di "F statistic = $fstat_4"
di "Pvalue = $pvalue_4"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score, "F statistic (a) = (b)", $fstat_2, "P value (a) = (b)", $pvalue_2 )	 /*
	*/	addtext() /*
	*/	tex(fragment) /*
	*/	replace

* Regressão 2
reg job_loss ///
educ1 educ2 educ3 educ4 ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(State FE, Yes, Urban FE, Yes) /*
	*/	tex(fragment) /*
	*/	

* Regressão 3
reg job_loss ///
educ1 educ2 educ3 educ4 ///
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(State FE, Yes, Urban FE, Yes, Time-varying observables, Yes) /*
	*/	tex(fragment) /*
	*/
	
* Regressão 4	
reg job_loss ///
educ1 educ2 educ3 educ4 ///
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

global rmse_score : di %9.3fc `e(rmse)' 
di "Root mean squared error = $rmse_score"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(State FE, Yes, Urban FE, Yes, Time-varying observables, Yes, Occupation and Sector FE, Yes) /*
	*/	tex(fragment) /*
	*/
	