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

* generate variable depicting the pandemic period (2020.2 - 2021.3)
gen pandemic_2020_2_2021_3 = 0
replace pandemic_2020_2_2021_3 = 1 if year_quarter >= 20202 & year_quarter <= 20213

gen pandemic_2020_2_2021_3_educ0 = 0
replace pandemic_2020_2_2021_3_educ0 = 1 if educ0 == 1 & year_quarter >= 20202 & year_quarter <= 20213

gen pandemic_2020_2_2021_3_educ1 = 0
replace pandemic_2020_2_2021_3_educ1 = 1 if educ1 == 1 & year_quarter >= 20202 & year_quarter <= 20213

label variable pandemic_2020_2_2021_3 "Pandemic 2020_2 - 2021_3"
label variable pandemic_2020_2_2021_3_educ0 "Pandemic 2020_2 - 2021_3 * No College"
label variable pandemic_2020_2_2021_3_educ1 "Pandemic 2020_2 - 2021_3 * Complete College"


* Regressão 1
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1 ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat1 = r(F) 
local fstat1 : display %9.3g `fstat1' 

local pvalue1 = r(p)
local pvalue1 : display %9.3fc `pvalue1' 

di "F statistic (2020.1) = `fstat1'"
di "Pvalue (2020.1) = `pvalue1'"

test educ0 + pandemic_2020_2_2021_3_educ0 = educ1 + pandemic_2020_2_2021_3_educ1

local fstat2 = r(F) 
local fstat2 : display %9.3g `fstat2' 

local pvalue2 = r(p)
local pvalue2 : display %9.3fc `pvalue2' 

di "F statistic (2020.2-2021.3) = `fstat2'"
di "Pvalue (2020.2-2021.3) = `pvalue2'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_educ0 pandemic_educ1 pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Stat: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat1'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue1'" ,  /*
	*/ 	"F Stat: No College + Pandemic 2020\_2-2021\_3*No College = Complete College + Pandemic 2020\_2-2021\_3*Complete College:", "`fstat2'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue2'" ,  /*
	*/ 	"State FE", "-", "Urban FE", "-", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	replace
		
* Regressão 2
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1 ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat1 = r(F) 
local fstat1 : display %9.3g `fstat1' 

local pvalue1 = r(p)
local pvalue1 : display %9.3fc `pvalue1' 

di "F statistic (2020.1) = `fstat1'"
di "Pvalue (2020.1) = `pvalue1'"

test educ0 + pandemic_2020_2_2021_3_educ0 = educ1 + pandemic_2020_2_2021_3_educ1

local fstat2 = r(F) 
local fstat2 : display %9.3g `fstat2' 

local pvalue2 = r(p)
local pvalue2 : display %9.3fc `pvalue2' 

di "F statistic (2020.2-2021.3) = `fstat2'"
di "Pvalue (2020.2-2021.3) = `pvalue2'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_educ0 pandemic_educ1 pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Stat: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat1'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue1'" ,  /*
	*/ 	"F Stat: No College + Pandemic 2020\_2-2021\_3*No College = Complete College + Pandemic 2020\_2-2021\_3*Complete College:", "`fstat2'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue2'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "-", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 3
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1 ///
i.gender i.race age ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat1 = r(F) 
local fstat1 : display %9.3g `fstat1' 

local pvalue1 = r(p)
local pvalue1 : display %9.3fc `pvalue1' 

di "F statistic (2020.1) = `fstat1'"
di "Pvalue (2020.1) = `pvalue1'"

test educ0 + pandemic_2020_2_2021_3_educ0 = educ1 + pandemic_2020_2_2021_3_educ1

local fstat2 = r(F) 
local fstat2 : display %9.3g `fstat2' 

local pvalue2 = r(p)
local pvalue2 : display %9.3fc `pvalue2' 

di "F statistic (2020.2-2021.3) = `fstat2'"
di "Pvalue (2020.2-2021.3) = `pvalue2'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_educ0 pandemic_educ1 pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Stat: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat1'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue1'" ,  /*
	*/ 	"F Stat: No College + Pandemic 2020\_2-2021\_3*No College = Complete College + Pandemic 2020\_2-2021\_3*Complete College:", "`fstat2'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue2'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
	
* Regressão 4	
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1 ///
i.gender i.race age ///
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

local fstat1 = r(F) 
local fstat1 : display %9.3g `fstat1' 

local pvalue1 = r(p)
local pvalue1 : display %9.3fc `pvalue1' 

di "F statistic (2020.1) = `fstat1'"
di "Pvalue (2020.1) = `pvalue1'"

test educ0 + pandemic_2020_2_2021_3_educ0 = educ1 + pandemic_2020_2_2021_3_educ1

local fstat2 = r(F) 
local fstat2 : display %9.3g `fstat2' 

local pvalue2 = r(p)
local pvalue2 : display %9.3fc `pvalue2' 

di "F statistic (2020.2-2021.3) = `fstat2'"
di "Pvalue (2020.2-2021.3) = `pvalue2'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_educ0 pandemic_educ1 pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Stat: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat1'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue1'" ,  /*
	*/ 	"F Stat: No College + Pandemic 2020\_2-2021\_3*No College = Complete College + Pandemic 2020\_2-2021\_3*Complete College:", "`fstat2'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue2'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/


* Regressão 5
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1 ///
signed_work_card i.job_function hours_worked temporary_worker social_security_taxpayer i.gender i.race age monthly_work_income i.job_start ///
i.year_quarter ///
i.state ///
i.urbana ///
[aw=weights] ///
, nocons

local rmse_score : di %9.3fc `e(rmse)'
local rmse_score : display %9.3fc `rmse_score' 
di "Root mean squared error = `rmse_score'"

test educ0 + pandemic_educ0 = educ1 + pandemic_educ1

local fstat1 = r(F) 
local fstat1 : display %9.3g `fstat1' 

local pvalue1 = r(p)
local pvalue1 : display %9.3fc `pvalue1' 

di "F statistic (2020.1) = `fstat1'"
di "Pvalue (2020.1) = `pvalue1'"

test educ0 + pandemic_2020_2_2021_3_educ0 = educ1 + pandemic_2020_2_2021_3_educ1

local fstat2 = r(F) 
local fstat2 : display %9.3g `fstat2' 

local pvalue2 = r(p)
local pvalue2 : display %9.3fc `pvalue2' 

di "F statistic (2020.2-2021.3) = `fstat2'"
di "Pvalue (2020.2-2021.3) = `pvalue2'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_educ0 pandemic_educ1 pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Stat: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat1'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue1'" ,  /*
	*/ 	"F Stat: No College + Pandemic 2020\_2-2021\_3*No College = Complete College + Pandemic 2020\_2-2021\_3*Complete College:", "`fstat2'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue2'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "-" )	 /* 
	*/	tex(fragment) /*
	*/	
	
* Regressão 6	
reg job_loss ///
educ0 educ1 ///
pandemic_educ0 pandemic_educ1 /// 
pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1 ///
signed_work_card i.job_function hours_worked temporary_worker social_security_taxpayer i.gender i.race age monthly_work_income i.job_start ///
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

local fstat1 = r(F) 
local fstat1 : display %9.3g `fstat1' 

local pvalue1 = r(p)
local pvalue1 : display %9.3fc `pvalue1' 

di "F statistic (2020.1) = `fstat1'"
di "Pvalue (2020.1) = `pvalue1'"

test educ0 + pandemic_2020_2_2021_3_educ0 = educ1 + pandemic_2020_2_2021_3_educ1

local fstat2 = r(F) 
local fstat2 : display %9.3g `fstat2' 

local pvalue2 = r(p)
local pvalue2 : display %9.3fc `pvalue2' 

di "F statistic (2020.2-2021.3) = `fstat2'"
di "Pvalue (2020.2-2021.3) = `pvalue2'"

outreg2	using "$ROOT/analysis/output/regressions/_table_regression_job_loss_determinants_with_interaction.tex", /*
	*/	title("") /*	
	*/	level(95) /*
	*/	dec(3) /*
	*/	fmt(fc) /*
	*/	label /*
	*/		/* depvar
	*/	keep(educ0 educ1 pandemic_educ0 pandemic_educ1 pandemic_2020_2_2021_3_educ0 pandemic_2020_2_2021_3_educ1)  /*
	*/	nocons	/*
	*/	addtext("Root mean squared error:", "`rmse_score'" ,  	/*
	*/ 	"F Stat: No College + Pandemic*No College = Complete College + Pandemic*Complete College:", "`fstat1'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue1'" ,  /*
	*/ 	"F Stat: No College + Pandemic 2020\_2-2021\_3*No College = Complete College + Pandemic 2020\_2-2021\_3*Complete College:", "`fstat2'" , /*
	*/ 	"\hspace{1mm} P-value:", "`pvalue2'" ,  /*
	*/ 	"State FE", "Yes", "Urban FE", "Yes", "Time-varying observables", "Yes", "Occupation and sector FE", "Yes" )	 /* 
	*/	tex(fragment) /*
	*/
