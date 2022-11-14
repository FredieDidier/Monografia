cd "C:/GitHub/Monografia"

use "input/regression_df.dta", clear

*** amostra aleatória pequena
***

* labels educ

label define educ_levels 1 "\makecell[l]{Incomplete\\ Primary School}" 2 "\makecell[l]{Incomplete\\ High School}" 3 "\makecell[l]{Incomplete\\ College}" 4 "\makecell[l]{Complete\\ College}"

label values educ educ_levels

* transformando em números

labmask year_quarter, values(year_quarter)

encode occupation_code, gen(occupation)
encode sector_code, gen(sector)
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

* genero, raça, idade, localização do domicilio, setor, ocupação, experiência, trimestre

qui mlogit transition i.educ homem negro urbana age i.sector i.occupation i.job_start i.year_quarter [aweight = weights], vce(robust)
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
tab educ, gen(educ_dx)
tab sector, gen(sector_dx)
tab occupation, gen(occupation_dx)
tab job_start, gen(job_start_dx)
tab year_quarter, gen(year_quarter_dx)

xtset ind year_quarter
qui femlogit transition educ_dx1 educ_dx2 educ_dx3 educ_dx4 urbana age sector_dx1 sector_dx2 sector_dx3 sector_dx4 sector_dx5 sector_dx6 sector_dx7 sector_dx8 sector_dx9 sector_dx10 sector_dx11 sector_dx12 sector_dx13 sector_dx14 sector_dx15 sector_dx16 sector_dx17 sector_dx18 sector_dx19 sector_dx20 sector_dx21 sector_dx22 sector_dx23 sector_dx24 occupation_dx1 occupation_dx2 occupation_dx3 occupation_dx4 occupation_dx5 occupation_dx6 occupation_dx7 occupation_dx8 occupation_dx9 occupation_dx10 job_start_dx1 job_start_dx2 job_start_dx3 job_start_dx4 year_quarter_dx1 year_quarter_dx2 year_quarter_dx3 year_quarter_dx4 year_quarter_dx5 year_quarter_dx6 year_quarter_dx7 year_quarter_dx8 year_quarter_dx9 year_quarter_dx10 year_quarter_dx11 year_quarter_dx12 year_quarter_dx13 year_quarter_dx14 year_quarter_dx15 year_quarter_dx16 year_quarter_dx17 year_quarter_dx18 year_quarter_dx19 year_quarter_dx20 year_quarter_dx21 year_quarter_dx22 year_quarter_dx23 year_quarter_dx24 year_quarter_dx25 year_quarter_dx26 year_quarter_dx27 year_quarter_dx28 year_quarter_dx29 year_quarter_dx30 year_quarter_dx31 year_quarter_dx32 year_quarter_dx33 year_quarter_dx34 year_quarter_dx35 year_quarter_dx36 year_quarter_dx37 year_quarter_dx38 year_quarter_dx39 year_quarter_dx40
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

* criando arquivo para armazenar os resultados
tempfile results_file

tempname results

postfile `results' year_quarter transition educ coef se using `results_file' // dataframe com essas colunas

foreach trim of numlist 20121/20124 20131/20134 20141/20144 20151/20154 20161/20164 20171/20174 20181/20184 20191/20194 20201/20204 20211/20214{

	preserve
	
	keep if year_quarter == `trim'
	
	di as input "Regressão de `trim'"
	
qui mlogit transition i.educ homem negro urbana age i.occupation i.job_start [aweight = weights], vce(robust)
	
	estimates store model_`trim'
	
	forvalues i = 0/5{
		estimates restore model_`trim'
		margins educ, predict(outcome(`i')) post
		
		matrix B = r(b) // armazenando matriz de coeficientes
		matrix V = r(V) // matriz de variâncias
		
		forvalues educ = 1/4{
			post `results' (`trim') (`i') (`educ') (B[1,`educ']) (V[`educ',`educ'])
		}
	}
	
	restore
}

postclose `results'

use `results_file', clear

replace se = sqrt(se)

save "input/reg_grafico.dta"
