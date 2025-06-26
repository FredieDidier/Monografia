* regressao: job loss

* Regressão 1
reg job_loss ///
educ0 educ1 /// 
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 = educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College = Complete College:", "`fstat'" , "\hspace{1mm} P-value No College = Complete College:", "`pvalue'" ,  /*
	*/ 	"State FE", "-", "Urban FE", "-", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	replace
		
* Regressão 2
reg job_loss ///
educ0 educ1 ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 = educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College = Complete College:", "`fstat'" , "\hspace{1mm} P-value No College = Complete College:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 3
reg job_loss ///
educ0 educ1 ///
gender race age ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 = educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College = Complete College:", "`fstat'" , "\hspace{1mm} P-value No College = Complete College:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 4
reg job_loss ///
educ0 educ1 ///
gender race age ///
i.year_quarter ///
i.state ///
i.urbana ///
i.sector_numeric i.occupation_numeric ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 = educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College = Complete College:", "`fstat'" , "\hspace{1mm} P-value No College = Complete College:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/	

* Regressão 5
reg job_loss ///
educ0 educ1 ///
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 = educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College = Complete College:", "`fstat'" , "\hspace{1mm} P-value No College = Complete College:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 6	
reg job_loss ///
educ0 educ1 ///
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 = educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College = Complete College:", "`fstat'" , "\hspace{1mm} P-value No College = Complete College:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/
