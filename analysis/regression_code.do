cd "C:/GitHub/Monografia"

use "input/regression.dta", clear

*** amostra aleatória pequena
sample 5
***

* labels educ

label define educ_levels 1 "Incomplete Primary School" 2 "Incomplete High School" 3 "Incomplete College" 4 "Complete College"

label values educ educ_levels

* tranformando em números

labmask year_quarter, values(year_quarter)

encode occupation_code, gen(occupation)

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

*** Lista de variáveis nas tabelas

local varlist _cons 2.educ 3.educ 4.educ 20191.year_quarter 20192.year_quarter 20193.year_quarter 20194.year_quarter 20201.year_quarter 20202.year_quarter 20203.year_quarter 20204.year_quarter 20211.year_quarter 20212.year_quarter 20213.year_quarter 20214.year_quarter 20221.year_quarter

///////////////////
// Sem controles //
///////////////////

qui mlogit transition i.educ i.year_quarter [aweight = weights], vce(robust)

esttab using "output/regression_1.tex", se label unstack noomitted stats(r2_p N, labels("Pseudo R-Squared" "N")) keep(`varlist') collab(none) mlabels(none) /*
	*/ eqlabels("Formal to Informal" "Formal to Non-Employed" "Informal to Formal" "Informal to Informal" "Informal to Non-Employed") /*
	*/ replace
	
///////////////////
// Controles 1   //
///////////////////

* genero, raça, idade, setor, exp, localização do domicilio, categoria de emprego, occ codes

qui mlogit transition i.educ homem negro urbana age i.job_start i.work_category i.occupation i.year_quarter [aweight = weights], vce(robust)

estout using "output/regression_2.tex", stats(r2_p N, labels("Pseudo R-Squared" "N")) cells(b(star fmt(3)) se(par fmt(3))) ///
	label unstack eqlabels("Formal to Formal" "Formal to Informal" "Formal to Non-Employed" "Informal to Formal" "Informal to Informal" "Informal to Non-Employed") ///
	drop(Formal_to_Formal:) keep(`varlist') collab(none) mlabels(none) replace

///////////////////
// Controles 2   //
///////////////////

* + efeitos fixos de individuo

qui mlogit transition i.educ homem negro urbana age i.job_start i.work_category i.occupation i.year_quarter i.id_code [aweight = weights], vce(robust)

estout using "output/regression_3.tex", stats(r2_p N, labels("Pseudo R-Squared" "N")) cells(b(star fmt(3)) se(par fmt(3))) ///
	label unstack eqlabels("Formal to Formal" "Formal to Informal" "Formal to Non-Employed" "Informal to Formal" "Informal to Informal" "Informal to Non-Employed") ///
	drop(Formal_to_Formal:) keep(`varlist') collab(none) mlabels(none) replace
