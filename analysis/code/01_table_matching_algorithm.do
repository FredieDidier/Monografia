preserve

clear

use "$ROOT/build/output/painel_2020_1.dta", clear
append using "$ROOT/build/output/painel_2020_2.dta", force
append using "$ROOT/build/output/painel_2020_3.dta", force

/*
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
	}

*/
		
keep Ano Trimestre UPA Estrato V1008 V1014 V1016 V1028 painel idind	
compress	

sort idind Ano Trimestre

collapse (firstnm) UPA Estrato V1008 V1014 V1016 V1028 painel, by(idind Ano Trimestre)

egen id_dom =group(UPA V1008 V1014)
	
* The share of overlap between pairs of quarters between the quarterly data of PNAD Continuous (in \%)
** Household vs. Individuals
** Time interval p Ano Trimestre UPA Estrato V1008 V1014 V1016 V1028 painel idind	
compress	

sort idind Ano Trimestre

collapse (firstnm) UPA Estrato V1008 V1014 V1016 V1028 painel, by(idind Ano Trimestre)

egen id_dom =group(UPA V1008 V1014)
	
* The share of overlap between pairs of quarters between the quarterly data of PNAD Continuous (in \%)
** Household vs. Individuals
** Time interval 1,2,3,4
** Accross panels 

* Individuals
cap drop id_appear_*
cap drop iten*

by idind, sort: gen iten1 = _n
by idind, sort: egen iten2 = max(iten1)
replace iten2 =. if iten1 >1

by painel, sort: egen id_appear_1_time = count(idind) if iten2 ==1
by painel, sort: egen id_appear_2_time = count(idind) if iten2 ==2
by painel, sort: egen id_appear_3_time = count(idind) if iten2 ==3
by painel, sort: egen id_appear_4_time = count(idind) if iten2 ==4
by painel, sort: egen id_appear_5_time = count(idind) if iten2 ==5
by painel, sort: egen id_appear_all = count(idind) if iten2 !=.

cap drop n_id_that_appear_*
by painel, sort: egen n_id_that_appear_1 = mode(id_appear_1_time)
replace n_id_that_appear_1 = 0 if n_id_that_appear_1==.
by painel, sort: egen n_id_that_appear_2 = mode(id_appear_2_time)
replace n_id_that_appear_2 = 0 if n_id_that_appear_2==.
by painel, sort: egen n_id_that_appear_3 = mode(id_appear_3_time)
replace n_id_that_appear_3 = 0 if n_id_that_appear_3==.
by painel, sort: egen n_id_that_appear_4 = mode(id_appear_4_time)
replace n_id_that_appear_4 = 0 if n_id_that_appear_4==.
by painel, sort: egen n_id_that_appear_5 = mode(id_appear_5_time)
replace n_id_that_appear_5 = 0 if n_id_that_appear_5==.

by painel, sort: egen n_id_that_appear_all = mode(id_appear_all) 
replace n_id_that_appear_all = 0 if n_id_that_appear_all==.

cap drop somatorio
cap drop diferenca
gen somatorio = n_id_that_appear_1 + n_id_that_appear_2 + n_id_that_appear_3 + n_id_that_appear_4 + n_id_that_appear_5
gen diferenca  = somatorio - n_id_that_appear_all
sum diferenca
cap drop diferenca somatorio

gen sh_id_between_pairs1 = (n_id_that_appear_1 / n_id_that_appear_all)*100
gen sh_id_between_pairs2 = (n_id_that_appear_2 / n_id_that_appear_all)*100
gen sh_id_between_pairs3 = (n_id_that_appear_3 / n_id_that_appear_all)*100
gen sh_id_between_pairs4 = (n_id_that_appear_4 / n_id_that_appear_all)*100
gen sh_id_between_pairs5 = (n_id_that_appear_5 / n_id_that_appear_all)*100

* Households
cap drop n_hs_that_appear_*
cap drop hs_that_appear_*
cap drop iten*

by id_dom Ano Trimestre, sort: gen iten1 = _n
replace iten1 =. if iten1 >1
gsort  id_dom iten1 Ano Trimestre
by id_dom iten1, sort: gen iten2 = _n if iten1 !=.

by id_dom Ano Trimestre, sort: egen hs_that_appear_1_time = max(iten2) if iten2 == 1
by id_dom Ano Trimestre, sort: egen hs_that_appear_2_time = max(iten2) if iten2 == 2 
by id_dom Ano Trimestre, sort: egen hs_that_appear_3_time = max(iten2) if iten2 == 3 
by id_dom Ano Trimestre, sort: egen hs_that_appear_4_time = max(iten2) if iten2 == 4 
by id_dom Ano Trimestre, sort: egen hs_that_appear_5_time = max(iten2) if iten2 == 5

browse id_dom iten1 Ano Trimestre iten* hs_that_appear_*

by painel, sort: egen n_hs_that_appear_1_time = count(iten1) if hs_that_appear_1_time ==1
by painel, sort: egen n_hs_that_appear_2_time = count(iten1) if hs_that_appear_2_time ==2
by painel, sort: egen n_hs_that_appear_3_time = count(iten1) if hs_that_appear_3_time ==3
by painel, sort: egen n_hs_that_appear_4_time = count(iten1) if hs_that_appear_4_time ==4
by painel, sort: egen n_hs_that_appear_5_time = count(iten1) if hs_that_appear_5_time ==5
by painel, sort: egen n_hs_that_appear_all = count(iten1) if iten2 !=.

by painel, sort: egen n_hs_1 = mode(n_hs_that_appear_1)
replace n_hs_1 = 0 if n_hs_that_appear_1==.
by painel, sort: egen n_hs_2 = mode(n_hs_that_appear_2)
replace n_hs_2 = 0 if n_hs_that_appear_2==.
by painel, sort: egen n_hs_3 = mode(n_hs_that_appear_3)
replace n_hs_3 = 0 if n_hs_that_appear_3==.
by painel, sort: egen n_hs_4 = mode(n_hs_that_appear_4)
replace n_hs_4 = 0 if n_hs_that_appear_4==.
by painel, sort: egen n_hs_5 = mode(n_hs_that_appear_5)
replace n_hs_5 = 0 if n_hs_that_appear_5==.
by painel, sort: egen n_hs_all = mode(n_hs_that_appear_all)
replace n_hs_all = 0 if n_hs_that_appear_all==.

gen sh_hs_between_pairs1 = (n_hs_1 / n_hs_all)*100
gen sh_hs_between_pairs2 = (n_hs_2 / n_hs_all)*100
gen sh_hs_between_pairs3 = (n_hs_3 / n_hs_all)*100
gen sh_hs_between_pairs4 = (n_hs_4 / n_hs_all)*100
gen sh_hs_between_pairs5 = (n_hs_5 / n_hs_all)*100

collapse (max ) sh_* , by(painel)

// tabulate
estimates clear
estpost sum  sh_hs_*

* TEX, prettier
#delim ;
esttab using "$ROOT/analysis/output/descriptive_statistics/_table_matching_algorithm.tex", replace
    cells
        (
        "
        sum( fmt(%12.0fc) label(Qty)) 			
        mean( fmt(%8.2fc) label(Share)) 
        "
        )     
    label 
    booktabs 
    type 
    nostar
    noobs
    nonumbers
    nomtitle  
    title("Matching")
    prehead(
        "\begin{table}"
        "\centering"
        "\caption{}"
        "\begin{tabular}{l*{@span}{r}}"
        "\hline \hline"
    )
    postfoot(
        "\hline \hline"
        "\end{tabular}"    
        "\end{table}"
    )
;  
#delim cr


*Losses between pairs of trimesters, following the Basic matching criteria (in \%)

*Losses between pairs of trimesters, following the Advanced matching criteria (in \%)
		
restore