* ------------------------------------------------------------------------

set more off

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

* 1. Replicar o trabalho do Journal of Public Economics "Inequality of the coronavirus shock".
* 2. Primeira analise: perda de emprego #grafico
do "$ROOT/analysis/code/_graph_job_loss_sectors.do"
* 2.1. Porcentagem de perda de emprego por setor/categoria de emprego e educacao. #graficos
* 3. Regressao para determinantes de perda de emprego e renda.
use "$ROOT/build/output/regression/main_data.dta", clear
sample 2
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

* 4. Regressao para determinantes de perda de emprego e renda: caracteristicas individuais.
* 5. Efeitos da industria para perder emprego (efeitos fixos) - grafico da regressao
* 6. Efeitos da ocupacao para perder emprego (efeitos fixos) - grafico da regressao


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
