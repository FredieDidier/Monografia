preserve

clear

/*
use "$ROOT/build/output/painel_2020_1.dta", clear
append using "$ROOT/build/output/painel_2020_2.dta", force
append using "$ROOT/build/output/painel_2020_3.dta", force
append using "$ROOT/build/output/painel_2020_4.dta", force
*/

* extract files .csv
clear
cd  "$ROOT/build/output"
pwd

* set files
local uniquefile : dir . files "painel_*.dta"
display `uniquefile'

foreach name_uniquefile in `uniquefile' {	
		*append using "${tmp_dir}/`yr'-`mn'-01_amc_rainfall.dta"
		append using "$ROOT/build/output/`name_uniquefile'", force
	}
		
keep Ano Trimestre UPA Estrato V1008 V1014 V1016 V1028 painel idind	
compress	

sort idind Ano Trimestre

collapse (firstnm) UPA Estrato V1008 V1014 V1016 V1028 painel, by(idind Ano Trimestre)

egen id_dom =group(UPA V1008 V1014)
	
*******************************************************************************************************	
* The share of overlap between pairs of quarters between the quarterly data of PNAD Continuous (in \%)
** Household vs. Individuals
*******************************************************************************************************

*******************************************************************************************************
* The share of overlap between pairs of quarters between the quarterly data of PNAD Continuous (in \%)
** Household vs. Individuals
** Time interval 1,2,3,4
** Accross panels 
*******************************************************************************************************
* Individuals

* This line drops all variables that start with "id_appear_"
cap drop id_appear_*

* This line drops all variables with the name "iten"
cap drop iten*

* From this point, the code works on creating new variables and performing calculations.

* Generates a unique identifier for each observation within each "idind" group.
by idind, sort: gen iten1 = _n

* Creates a variable "iten2" that contains the maximum value of "iten1" within each "idind" group.
by idind, sort: egen iten2 = max(iten1)

* Sets "iten2" to missing (.) for observations where "iten1" is greater than 1.
replace iten2 = . if iten1 > 1

* Creates new variables counting the number of times each "idind" appears in different "painel" groups.
by painel, sort: egen id_appear_1_time = count(idind) if iten2 == 1
by painel, sort: egen id_appear_2_time = count(idind) if iten2 == 2
by painel, sort: egen id_appear_3_time = count(idind) if iten2 == 3
by painel, sort: egen id_appear_4_time = count(idind) if iten2 == 4
by painel, sort: egen id_appear_5_time = count(idind) if iten2 == 5
by painel, sort: egen id_appear_all = count(idind) if iten2 != .

* Drops temporary variables starting with "n_id_that_appear_"
cap drop n_id_that_appear_*

* Calculates the mode (most frequent value) of "id_appear_1_time" within each "painel" group.
* Sets cases where the mode is missing (.) to zero (0).
by painel, sort: egen n_id_that_appear_1 = mode(id_appear_1_time)
replace n_id_that_appear_1 = 0 if n_id_that_appear_1 == .
by painel, sort: egen n_id_that_appear_2 = mode(id_appear_2_time)
replace n_id_that_appear_2 = 0 if n_id_that_appear_2 == .
by painel, sort: egen n_id_that_appear_3 = mode(id_appear_3_time)
replace n_id_that_appear_3 = 0 if n_id_that_appear_3 == .
by painel, sort: egen n_id_that_appear_4 = mode(id_appear_4_time)
replace n_id_that_appear_4 = 0 if n_id_that_appear_4 == .
by painel, sort: egen n_id_that_appear_5 = mode(id_appear_5_time)
replace n_id_that_appear_5 = 0 if n_id_that_appear_5 == .

* Repeats the process for the "id_appear_all" variable.
by painel, sort: egen n_id_that_appear_all = mode(id_appear_all)
replace n_id_that_appear_all = 0 if n_id_that_appear_all == .

* Drops temporary variables "somatorio" and "diferenca" if they already exist.
cap drop somatorio
cap drop diferenca

* Calculates the sum of variables "n_id_that_appear_1" to "n_id_that_appear_5" and assigns the result to "somatorio."
* Calculates the difference between "somatorio" and "n_id_that_appear_all" and assigns the result to "diferenca."
* Displays a statistical summary of the "diferenca" variable.
gen somatorio = n_id_that_appear_1 + n_id_that_appear_2 + n_id_that_appear_3 + n_id_that_appear_4 + n_id_that_appear_5
gen diferenca = somatorio - n_id_that_appear_all
sum diferenca
cap drop diferenca somatorio

* Calculates the percentages of "n_id_that_appear_1" to "n_id_that_appear_5" relative to "n_id_that_appear_all" 
* and stores these percentages in new variables "sh_id_between_pairs1" to "sh_id_between_pairs5."
gen sh_id_between_pairs1 = (n_id_that_appear_1 / n_id_that_appear_all) * 100
gen sh_id_between_pairs2 = (n_id_that_appear_2 / n_id_that_appear_all) * 100
gen sh_id_between_pairs3 = (n_id_that_appear_3 / n_id_that_appear_all) * 100
gen sh_id_between_pairs4 = (n_id_that_appear_4 / n_id_that_appear_all) * 100
gen sh_id_between_pairs5 = (n_id_that_appear_5 / n_id_that_appear_all) * 100

* Households
* Drop variables starting with "n_hs_that_appear_"
* Drop variables starting with "hs_that_appear_"
* Drop variables containing "iten"
cap drop n_hs_that_appear_*
cap drop hs_that_appear_*
cap drop iten*

* Generate a unique identifier for each observation within groups defined by "id_dom," "Ano," and "Trimestre."
by id_dom Ano Trimestre, sort: gen iten1 = _n

* Set "iten1" to missing (.) for observations where it's greater than 1.
replace iten1 = . if iten1 > 1

* Sort the data by "id_dom," "iten1," "Ano," and "Trimestre."
gsort id_dom iten1 Ano Trimestre

* Generate a new identifier "iten2" within each "id_dom" group if "iten1" is not missing.
by id_dom iten1, sort: gen iten2 = _n if iten1 != .

* Creates a variable "iten3" that contains the maximum value of "iten2" within each "id_dom" group.
by id_dom, sort: egen iten3 = max(iten2) if iten1 != .

* Calculate the maximum value of "iten2" within each "id_dom," "Ano," and "Trimestre" group, ]
* and store it in "hs_that_appear_1_time" to "hs_that_appear_5_time" based on the value of "iten2."
by id_dom Ano Trimestre, sort: egen hs_that_appear_1_time = max(iten2) if iten2 == 1 & iten3 == 1
by id_dom Ano Trimestre, sort: egen hs_that_appear_2_time = max(iten2) if iten2 == 2 & iten3 == 2 
by id_dom Ano Trimestre, sort: egen hs_that_appear_3_time = max(iten2) if iten2 == 3 & iten3 == 3 
by id_dom Ano Trimestre, sort: egen hs_that_appear_4_time = max(iten2) if iten2 == 4 & iten3 == 4 
by id_dom Ano Trimestre, sort: egen hs_that_appear_5_time = max(iten2) if iten2 == 5 & iten3 == 5

* Browse the data to explore the variables "id_dom," "iten1," "Ano," "Trimestre," "iten*," and "hs_that_appear_*."

* Calculate the count of "iten1" within each "painel" group if "hs_that_appear_1_time" is equal to 1, 
* and similarly for "hs_that_appear_2_time" to "hs_that_appear_5_time."
by painel, sort: egen n_hs_that_appear_1_time = count(iten1) if hs_that_appear_1_time == 1
by painel, sort: egen n_hs_that_appear_2_time = count(iten1) if hs_that_appear_2_time == 2
by painel, sort: egen n_hs_that_appear_3_time = count(iten1) if hs_that_appear_3_time == 3
by painel, sort: egen n_hs_that_appear_4_time = count(iten1) if hs_that_appear_4_time == 4
by painel, sort: egen n_hs_that_appear_5_time = count(iten1) if hs_that_appear_5_time == 5

* Calculate the count of "iten1" within each "painel" group if "iten2" is not missing.
by painel, sort: egen n_hs_that_appear_all = count(iten2) if iten2 == 1

* Calculate the mode of "n_hs_that_appear_1" within each "painel" group and set it to 0 if it's missing.
by painel, sort: egen n_hs_1 = mode(n_hs_that_appear_1)
replace n_hs_1 = 0 if n_hs_that_appear_1 == .
by painel, sort: egen n_hs_2 = mode(n_hs_that_appear_2)
replace n_hs_2 = 0 if n_hs_that_appear_2 == .
by painel, sort: egen n_hs_3 = mode(n_hs_that_appear_3)
replace n_hs_3 = 0 if n_hs_that_appear_3 == .
by painel, sort: egen n_hs_4 = mode(n_hs_that_appear_4)
replace n_hs_4 = 0 if n_hs_that_appear_4 == .
by painel, sort: egen n_hs_5 = mode(n_hs_that_appear_5)
replace n_hs_5 = 0 if n_hs_that_appear_5 == .

* Calculate the mode of "n_hs_that_appear_all" within each "painel" group and set it to 0 if it's missing.
by painel, sort: egen n_hs_all = mode(n_hs_that_appear_all)
replace n_hs_all = 0 if n_hs_that_appear_all == .

* save temporary data to retrive later to check (if needed)
save "$ROOT/analysis/tmp/matching_before_collapse.dta", replace

collapse (max ) sh_* n_id_that_appear* n_hs_1 n_hs_2 n_hs_3 n_hs_4 n_hs_5 n_hs_all, by(painel)

* save temporary data to retrive later to check (if needed)
save "$ROOT/analysis/tmp/matching_after_collapse.dta", replace
use "$ROOT/analysis/tmp/matching_after_collapse.dta", clear

* Calculate the percentages of "n_hs_1" to "n_hs_5" in relation to "n_hs_all" 
* and store them in "sh_hs_between_pairs1" to "sh_hs_between_pairs5."
cap drop sh_hs_between_*
gen sh_hs_between_pairs1 = (n_hs_1 / n_hs_all) * 100
gen sh_hs_between_pairs2 = (n_hs_2 / n_hs_all) * 100
gen sh_hs_between_pairs3 = (n_hs_3 / n_hs_all) * 100
gen sh_hs_between_pairs4 = (n_hs_4 / n_hs_all) * 100
gen sh_hs_between_pairs5 = (n_hs_5 / n_hs_all) * 100

format %9.2f sh_id_between_pairs1
format %9.2f sh_id_between_pairs2
format %9.2f sh_id_between_pairs3
format %9.2f sh_id_between_pairs4
format %9.2f sh_id_between_pairs5

format %9.2f sh_hs_between_pairs1
format %9.2f sh_hs_between_pairs2
format %9.2f sh_hs_between_pairs3
format %9.2f sh_hs_between_pairs4
format %9.2f sh_hs_between_pairs5

* export data for share of individuals
 export delimited ///
 painel sh_hs_between_pairs1 sh_hs_between_pairs2 sh_hs_between_pairs3 sh_hs_between_pairs4 sh_hs_between_pairs5 ///
 using "$ROOT/analysis/output/descriptive_statistics/_table_matching_algorithm_share_households.csv", replace 

 * export data for number of individuals
export delimited ///
painel n_id_that_appear_1 n_id_that_appear_2 n_id_that_appear_3 n_id_that_appear_4 n_id_that_appear_5 n_id_that_appear_all  ///
using "$ROOT/analysis/output/descriptive_statistics/_table_matching_algorithm_number_individuals.csv", replace
 	
* export data for share of households
export delimited ///
painel sh_id_between_pairs1 sh_id_between_pairs2 sh_id_between_pairs3 sh_id_between_pairs4 sh_id_between_pairs5  ///
using "$ROOT/analysis/output/descriptive_statistics/_table_matching_algorithm_share_individuals.csv", replace

* export data for number of households
 export delimited ///
 painel n_hs_1 n_hs_2 n_hs_3 n_hs_4 n_hs_5 n_hs_all ///
 using "$ROOT/analysis/output/descriptive_statistics/_table_matching_algorithm_number_households.csv", replace 

*Losses between pairs of trimesters, following the Basic matching criteria (in \%)

*Losses between pairs of trimesters, following the Advanced matching criteria (in \%)
		
restore