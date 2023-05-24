
* regressao: job loss

global CONTROLS workforce_condition worker signed_work_card cnpj job_function hours_worked temporary_worker occupation_condition position social_security_taxpayer higher_educ_level work_category gender race age years_of_study monthly_work_income weights job_start educ urbana 

reghdfe job_loss ///
position ///
signed_work_card /// 
hours_worked /// i.job_function 
i.temporary_worker /// i.job_function 
i.educ /// 
i.work_category /// 
monthly_work_income ///
urbana ///
job_start ///
i.year_quarter ///
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
	*/	keep()  /*
	*/	nocons	/*
	*/	addstat("Root mean squared error", $rmse_score )	 /*
	*/	addtext(Ind FE, Yes, Ind FE, Yes) /*
	*/	tex(fragment) /*
	*/	replace
