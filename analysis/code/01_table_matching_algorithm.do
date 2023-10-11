preserve

clear

use "$ROOT/build/output/painel_2012_1.dta", clear
keep Ano Trimestre painel idind

* extract files .csv
clear
cd  "$ROOT/build/output"
pwd

* set files
local uniquefile : dir . files "*.dta"
display `uniquefile'

foreach name_uniquefile in `uniquefile' {	
		*append using "${tmp_dir}/`yr'-`mn'-01_amc_rainfall.dta"
		append using "$ROOT/build/output/`name_uniquefile'", force
		keep Ano Trimestre UPA Estrato V1008 V1014 V1016 V1028 painel idind
	}

compress	

sort idind Ano Trimestre

collapse (firstnm) UPA Estrato V1008 V1014 V1016 V1028, by(idind Ano Trimestre)
	
*The share of overlap between pairs of quarters between the quarterly data of PNAD Continuous (in \%)
** Household vs. Individuals
** Time interval 1,2,3,4
** Accross panels 
by idind, sort: gen iten1 = _n
by idind, sort: egen iten2 = max(iten1)

egen n_that_appear_1_time = count(idind) if iten2 ==1
egen n_that_appear_2_time = count(idind) if iten2 ==2
egen n_that_appear_3_time = count(idind) if iten2 ==3
egen n_that_appear_4_time = count(idind) if iten2 ==4
egen n_that_appear_5_time = count(idind) if iten2 ==5

caop drop iten*


*Losses between pairs of trimesters, following the Basic matching criteria (in \%)

*Losses between pairs of trimesters, following the Advanced matching criteria (in \%)
		
restore