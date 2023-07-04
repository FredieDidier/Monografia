* ------------------------------------------------------------------------

cap drop macro all
set more off, permanently

*** FOLDERS PATHWAY

* check what your username is in Stata by typing "di c(username)"
if "`c(username)'" == "Francisco"   {
	version 16.1
	global ROOT "C:/Users/Francisco/Dropbox/Research"
}
else if "`c(username)'" == "DELL"   {
	version 16.1
	global ROOT "C:\Users\DELL\Documents\GitHub\Monografia-Fredie"
}
else if "`c(username)'" == "f.cavalcanti"   {
	version 16.1
	global ROOT "C:/Users/f.cavalcanti/Documents/GitHub/"
}
else if "`c(username)'" == "Fredie"   {
	version 14.1
	global ROOT "/Users/Fredie/Documents/GitHub/Monografia"
}

cd $ROOT

* generate temporary folder
capture mkdir "$ROOT/analysis/tmp"
cap mkdir "$ROOT/analysis/input"
cap mkdir "$ROOT/analysis/output"
cap mkdir "$ROOT/analysis/output/graph"
cap mkdir "$ROOT/analysis/output/regressions"

* 1. Replicar o trabalho do Journal of Public Economics "Inequality of the coronavirus shock".
* 2. Primeira analise: perda de emprego #grafico
*do "$ROOT/analysis/code/_graph_job_loss_sectors.do" // OLHAR NO R
* 2.1. Porcentagem de perda de emprego por setor/categoria de emprego e educacao. #graficos
* 3. Regressao para determinantes de perda de emprego e renda.
use "$ROOT/build/output/regression/main_data.dta", clear
gen denominador = 1 if position_names == "Formal"
replace denominador = 1 if position_names == "Informal"

gen numerador = 1 if position_transition == "Formal to Non-Employed"
replace numerador = 1 if position_transition == "Informal to Non-Employed"

gen item1 = numerador * weights
gen item2 = denominador * weights
cap drop job_loss
gen job_loss = .
replace job_loss = 0 if denominador == 1
replace job_loss = 1 if numerador == 1
egen ind = group(id_code)
xtset ind year_quarter
* generate variable of quartely date
*tostring year_quarter, replace
*gen quarter = substr(year_quarter, 5, 1)
*gen year = substr(year_quarter, 1, 4)
*gen iten1 = year + "." + quarter
*gen  trim = quarterly(iten1, "YQ")
*drop iten*
*format trim %tm
*destring id_code, replace
*xtset id_code trim

global CONTROLS signed_work_card cnpj job_function hours_worked temporary_worker position social_security_taxpayer gender race age monthly_work_income weights job_start educ urbana state

* edit indpendent variables
cap drop educ1
gen educ1 = 1 if educ ==1 
replace educ1 = 0 if educ1 ==.
label variable educ1 "a) Incomplete primary school"

cap drop educ2
gen educ2 = 1 if educ ==2
replace educ2 = 0 if educ2 ==.
label variable educ2 "b) Incomplete high school"

cap drop educ3
gen educ3 = 1 if educ ==3
replace educ3 = 0 if educ3 ==.
label variable educ3 "c) Incomplete college"

cap drop educ4
gen educ4 = 1 if educ ==4
replace educ4 = 0 if educ4 ==.
label variable educ4 "d) Complete college"

cap drop sector_numeric
cap drop occupation_numeric
encode sector_code, gen(sector_numeric)
encode occupation_code, gen(occupation_numeric)


*  Table: Probability of Job Loss Relative to Least Educated Individuals by Educational Level Categories
do "$ROOT/analysis/code/06_table_regression_job_loss_determinants.do"

*  Table: Probability of Job Loss Relative to Least Educated Individuals During Pandemic by Educational Level Categories
do "$ROOT/analysis/code/06_table_regression_job_loss_determinants_with_interaction.do"

*  Graph: Probability of Job Loss Relative to Least Educated Individuals by Educational Level Categories
do "$ROOT/analysis/code/07_graph_regression_job_loss_determinants.do"

*do "$ROOT/analysis/code/08_graph_regression_job_loss_determinants_fixed_effect_model.do"

*


********************************************************
**	delete temporary files
********************************************************

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.dta"
foreach datafile of local datafiles {
        rm `datafile'
}

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.csv"
foreach datafile of local datafiles {
        rm `datafile'
}

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.txt"
foreach datafile of local datafiles {
        rm "`datafile'"
}

cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.nc"
foreach datafile of local datafiles {
        rm "`datafile'"
}


cd  "${tmp_dir}/"
local datafiles: dir "${tmp_dir}/" files "*.pdf"
foreach datafile of local datafiles {
        rm `datafile'
}

* clear all
clear
