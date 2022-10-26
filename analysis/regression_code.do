cd "C:/GitHub/Monografia"

use "input/regression.dta", clear

*** amostra aleatória pequena
***

* labels educ

label define educ_levels 1 "\makecell[l]{Incomplete\\ Primary School}" 2 "\makecell[l]{Incomplete\\ High School}" 3 "\makecell[l]{Incomplete\\ College}" 4 "\makecell[l]{Complete\\ College}"

label values educ educ_levels

* transformando em números

labmask year_quarter, values(year_quarter)

encode occupation_code, gen(occupation)
encode sector_code, gen(sector)
destring id_code, replace
egen ind = group(id_code)

gen transition = .

replace transition = 0 if position_transition == "Formal to Formal"
replace transition = 1 if position_transition == "Formal to Informal"
replace transition = 2 if position_transition == "Formal to Non-Employed"
replace transition = 3 if position_transition == "Informal to Formal"
replace transition = 4 if position_transition == "Informal to Informal"
replace transition = 5 if position_transition == "Informal to Non-Employed"
replace transition = 6 if position_transition == "Non-Employed to Formal"
replace transition = 7 if position_transition == "Non-Employed to Informal"
replace transition = 8 if position_transition == "Non-Employed to Non-Employed"

labmask transition, values(position_transition)

drop if transition >= 6

replace occupation = 10 if position_names == "Non-Employed"
replace work_category = 10 if position_names == "Non-Employed"
replace job_start = 5 if position_names == "Non-Employed"

///////////////////
// Sem controles //
///////////////////

qui mlogit transition i.educ [aweight = weights], vce(robust)
estimates store model1

forvalues i = 0/5{
	di as input "Regressão `i'/5"

	estimates restore model1
	qui eststo m`i': margins i.educ, predict(outcome(`i')) post
}

esttab m0 m1 m2 m3 m4 m5 using "output/regression_1.tex", b(3) se(3) label noomitted stats(N, labels("N") fmt(%9.0fc)) collab(none) /*
	*/ mtitle("\makecell[c]{Formal\\to\\Formal}" /*
	*/ "\makecell[c]{Formal\\to\\Informal}" /*
	*/ "\makecell[c]{Formal\\to\\Non-Employed}" /*
	*/ "\makecell[c]{Informal\\to\\Formal}" /*
	*/ "\makecell[c]{Informal\\to\\Informal}" /*
	*/ "\makecell[c]{Informal\\to\\Non-Employed}") /*
	*/ replace
	
///////////////////
// Controles 1   //
///////////////////

* genero, raça, idade, setor, localização do domicilio, trimestre

qui mlogit transition i.educ homem negro urbana age i.year_quarter [aweight = weights], vce(robust)
estimates store model2

forvalues i = 0/5{
	di as input "Regressão `i'/5"

	estimates restore model2
	qui eststo m`i': margins i.educ, predict(outcome(`i')) post
}

esttab m0 m1 m2 m3 m4 m5 using "output/regression_2.tex", b(3) se(3) label noomitted stats(N, labels("N") fmt(%9.0fc)) collab(none) /*
	*/ mtitle("\makecell[c]{Formal\\to\\Formal}" /*
	*/ "\makecell[c]{Formal\\to\\Informal}" /*
	*/ "\makecell[c]{Formal\\to\\Non-Employed}" /*
	*/ "\makecell[c]{Informal\\to\\Formal}" /*
	*/ "\makecell[c]{Informal\\to\\Informal}" /*
	*/ "\makecell[c]{Informal\\to\\Non-Employed}") /*
	*/ replace

///////////////////
// Controles 2   //
///////////////////

* + efeitos fixos de individuo

qui mlogit transition i.educ homem negro urbana age i.year_quarter ind [aweight = weights], vce(robust)
estimates store model3

forvalues i = 0/5{
	di as input "Regressão `i'/5"

	estimates restore model3
	qui eststo m`i': margins i.educ, predict(outcome(`i')) post
}

esttab m0 m1 m2 m3 m4 m5 using "output/regression_3.tex", b(3) se(3) label noomitted stats(N, labels("N") fmt(%9.0fc)) collab(none) /*
	*/ mtitle("\makecell[c]{Formal\\to\\Formal}" /*
	*/ "\makecell[c]{Formal\\to\\Informal}" /*
	*/ "\makecell[c]{Formal\\to\\Non-Employed}" /*
	*/ "\makecell[c]{Informal\\to\\Formal}" /*
	*/ "\makecell[c]{Informal\\to\\Informal}" /*
	*/ "\makecell[c]{Informal\\to\\Non-Employed}") /*
	*/ replace

************************
* Gráfico por ano      *
************************

qui mlogit transition i.educ i.year_quarter i.educ#i.year_quarter homem negro urbana age ind [aweight = weights], vce(robust)