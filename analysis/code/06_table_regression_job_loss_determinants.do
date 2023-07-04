* regressao: job loss

* Regressão 1
reg job_loss ///
educ1 educ2 educ3 educ4 /// 
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ1 = educ2

local fstat_2 = r(F) 
local fstat_2 : display %9.3g `fstat_2' 

local pvalue_2 = r(p)
local pvalue_2 : display %9.3fc `pvalue_2' 

di "F statistic = `fstat_2'"
di "Pvalue = `pvalue_2'"

test educ2 = educ3

local fstat_3 = r(F)
local fstat_3 : display %9.3g `fstat_3' 

local pvalue_3 = r(p)
local pvalue_3 : display %9.3fc `pvalue_3' 

di "F statistic = `fstat_3'"
di "Pvalue = `pvalue_3'"

test educ3 = educ4

local fstat_4 = r(F)
local fstat_4 : display %9.3g `fstat_4' 

local pvalue_4 = r(p)
local pvalue_4 : display %9.3fc `pvalue_4' 

di "F statistic = `fstat_4'"
di "Pvalue = `pvalue_4'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: a = b:", "`fstat_2'" , "\hspace{1mm} P-value a = b:", "`pvalue_2'" ,  /*
	*/ 	"F Statistic: b = c:", "`fstat_3'" , "\hspace{1mm} P-value b = c:", "`pvalue_3'" ,  /*
	*/ 	"F Statistic: c = d:", "`fstat_4'" , "\hspace{1mm} P-value c = d:", "`pvalue_4'" ,  /*
	*/ 	"State FE", "-", "Urban FE", "-", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
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

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ1 = educ2

local fstat_2 = r(F) 
local fstat_2 : display %9.3g `fstat_2' 

local pvalue_2 = r(p)
local pvalue_2 : display %9.3fc `pvalue_2' 

di "F statistic = `fstat_2'"
di "Pvalue = `pvalue_2'"

test educ2 = educ3

local fstat_3 = r(F)
local fstat_3 : display %9.3g `fstat_3' 

local pvalue_3 = r(p)
local pvalue_3 : display %9.3fc `pvalue_3' 

di "F statistic = `fstat_3'"
di "Pvalue = `pvalue_3'"

test educ3 = educ4

local fstat_4 = r(F)
local fstat_4 : display %9.3g `fstat_4' 

local pvalue_4 = r(p)
local pvalue_4 : display %9.3fc `pvalue_4' 

di "F statistic = `fstat_4'"
di "Pvalue = `pvalue_4'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: a = b:", "`fstat_2'" , "\hspace{1mm} P-value a = b:", "`pvalue_2'" ,  /*
	*/ 	"F Statistic: b = c:", "`fstat_3'" , "\hspace{1mm} P-value b = c:", "`pvalue_3'" ,  /*
	*/ 	"F Statistic: c = d:", "`fstat_4'" , "\hspace{1mm} P-value c = d:", "`pvalue_4'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
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

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ1 = educ2

local fstat_2 = r(F) 
local fstat_2 : display %9.3g `fstat_2' 

local pvalue_2 = r(p)
local pvalue_2 : display %9.3fc `pvalue_2' 

di "F statistic = `fstat_2'"
di "Pvalue = `pvalue_2'"

test educ2 = educ3

local fstat_3 = r(F)
local fstat_3 : display %9.3g `fstat_3' 

local pvalue_3 = r(p)
local pvalue_3 : display %9.3fc `pvalue_3' 

di "F statistic = `fstat_3'"
di "Pvalue = `pvalue_3'"

test educ3 = educ4

local fstat_4 = r(F)
local fstat_4 : display %9.3g `fstat_4' 

local pvalue_4 = r(p)
local pvalue_4 : display %9.3fc `pvalue_4' 

di "F statistic = `fstat_4'"
di "Pvalue = `pvalue_4'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: a = b:", "`fstat_2'" , "\hspace{1mm} P-value a = b:", "`pvalue_2'" ,  /*
	*/ 	"F Statistic: b = c:", "`fstat_3'" , "\hspace{1mm} P-value b = c:", "`pvalue_3'" ,  /*
	*/ 	"F Statistic: c = d:", "`fstat_4'" , "\hspace{1mm} P-value c = d:", "`pvalue_4'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
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

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ1 = educ2

local fstat_2 = r(F) 
local fstat_2 : display %9.3g `fstat_2' 

local pvalue_2 = r(p)
local pvalue_2 : display %9.3fc `pvalue_2' 

di "F statistic = `fstat_2'"
di "Pvalue = `pvalue_2'"

test educ2 = educ3

local fstat_3 = r(F)
local fstat_3 : display %9.3g `fstat_3' 

local pvalue_3 = r(p)
local pvalue_3 : display %9.3fc `pvalue_3' 

di "F statistic = `fstat_3'"
di "Pvalue = `pvalue_3'"

test educ3 = educ4

local fstat_4 = r(F)
local fstat_4 : display %9.3g `fstat_4' 

local pvalue_4 = r(p)
local pvalue_4 : display %9.3fc `pvalue_4' 

di "F statistic = `fstat_4'"
di "Pvalue = `pvalue_4'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ1 educ2 educ3 educ4)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: a = b:", "`fstat_2'" , "\hspace{1mm} P-value a = b:", "`pvalue_2'" ,  /*
	*/ 	"F Statistic: b = c:", "`fstat_3'" , "\hspace{1mm} P-value b = c:", "`pvalue_3'" ,  /*
	*/ 	"F Statistic: c = d:", "`fstat_4'" , "\hspace{1mm} P-value c = d:", "`pvalue_4'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/	
	