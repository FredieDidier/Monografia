* regressao: job loss

* generate variable depicting the pandemic period
cap drop pandemic*
gen pandemic = 0
replace pandemic = 1 if year_quarter >= 20201 & year_quarter <= 20201

gen pandemic_educ0 = 0
replace pandemic_educ0 = 1 if educ0 == 1 & year_quarter >= 20201 & year_quarter <= 20201

gen pandemic_educ1 = 0
replace pandemic_educ1 = 1 if educ1 == 1 & year_quarter >= 20201 & year_quarter <= 20201

label variable pandemic "Pandemic"
label variable pandemic_educ0 "Pandemic * No College"
label variable pandemic_educ1 "Pandemic * Complete College"

* generate variable depicting the pandemic period (2020.2)
gen pandemic_2020_2 = 0
replace pandemic_2020_2 = 1 if year_quarter == 20202

gen pandemic_2020_2_educ0 = 0
replace pandemic_2020_2_educ0 = 1 if educ0 == 1 & year_quarter == 20202

gen pandemic_2020_2_educ1 = 0
replace pandemic_2020_2_educ1 = 1 if educ1 == 1 & year_quarter == 20202

label variable pandemic "Pandemic"
label variable pandemic_2020_2 "Pandemic 2020_2"
label variable pandemic_educ0 "Pandemic * No College"
label variable pandemic_2020_2_educ0 "Pandemic 2020_2 * No College"
label variable pandemic_educ1 "Pandemic * Complete College"
label variable pandemic_2020_2_educ1 "Pandemic 2020_2 * Complete College"


* Regressão 1
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic pandemic_educ0 pandemic_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "-", "Urban FE", "-", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	replace
		
* Regressão 2
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic pandemic_educ0 pandemic_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 3
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
gender race age ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic pandemic_educ0 pandemic_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
	
* Regressão 4	
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
gender race age ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic pandemic_educ0 pandemic_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/


* Regressão 5
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic pandemic_educ0 pandemic_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 6	
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
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

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic pandemic_educ0 pandemic_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/
	
**************

* Regressão 7
reg job_loss ///
educ0 educ1 ///
pandemic_2020_2_educ0 pandemic_2020_2_educ1 /// 
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_2020_2_educ0 = educ1 + pandemic_2020_2_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction_2020_2.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_2020_2_educ0 pandemic_2020_2_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic 2020_2*No College = Complete College + Pandemic 2020_2*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "-", "Urban FE", "-", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	replace
		
* Regressão 8
reg job_loss ///
educ0 educ1 ///
pandemic_2020_2_educ0 pandemic_2020_2_educ1 /// 
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_2020_2_educ0 = educ1 + pandemic_2020_2_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction_2020_2.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_2020_2_educ0 pandemic_2020_2_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic 2020_2*No College = Complete College + Pandemic 2020_2*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 9
reg job_loss ///
educ0 educ1 ///
pandemic_2020_2_educ0 pandemic_2020_2_educ1 /// 
gender race age ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_2020_2_educ0 = educ1 + pandemic_2020_2_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction_2020_2.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_2020_2_educ0 pandemic_2020_2_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic 2020_2*No College = Complete College + Pandemic 2020_2*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
	
* Regressão 10	
reg job_loss ///
educ0 educ1 ///
pandemic_2020_2_educ0 pandemic_2020_2_educ1 /// 
 gender race age ///
i.year_quarter ///
i.sector_numeric i.occupation_numeric ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_2020_2_educ0 = educ1 + pandemic_2020_2_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction_2020_2.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_2020_2_educ0 pandemic_2020_2_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic 2020_2*No College = Complete College + Pandemic 2020_2*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/


* Regressão 11
reg job_loss ///
educ0 educ1 ///
pandemic_2020_2_educ0 pandemic_2020_2_educ1 /// 
signed_work_card job_function hours_worked temporary_worker social_security_taxpayer gender race age monthly_work_income job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_2020_2_educ0 = educ1 + pandemic_2020_2_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction_2020_2.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_2020_2_educ0 pandemic_2020_2_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic 2020_2*No College = Complete College + Pandemic 2020_2*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 12
reg job_loss ///
educ0 educ1 ///
pandemic_2020_2_educ0 pandemic_2020_2_educ1 /// 
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

test educ0 + pandemic_2020_2_educ0 = educ1 + pandemic_2020_2_educ1

local fstat = r(F) 
local fstat : display %9.3g `fstat' 

local pvalue = r(p)
local pvalue : display %9.3fc `pvalue' 

di "F statistic = `fstat'"
di "Pvalue = `pvalue'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction_2020_2.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_2020_2_educ0 pandemic_2020_2_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Statistic: No College + Pandemic 2020_2*No College = Complete College + Pandemic 2020_2*Complete College:", "`fstat'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/
	
	

	
	
