
* generate folder
cap rm "$ROOT/build/output/painel/pnadcontinua"
cap erase  "$ROOT/build/output/painel/pnadcontinua"
cap erase  "$ROOT/build/output/painel/pnadcontinua_1"
cap erase  "$ROOT/build/output/painel/pnadcontinua_2"
cap mkdir "$ROOT/build/output/painel"

* Gerando painel

datazoom_pnadcontinua, years(2012/2024) ///
					original("$ROOT_DATA") ///
					saving("$ROOT/build/output/painel") ///
					idrs
					
* Gerando arquivos separados por pares de trimestres

set more off

cd $ROOT

local files PNAD_painel_1_rs PNAD_painel_2_rs PNAD_painel_3_rs PNAD_painel_4_rs ///
    PNAD_painel_5_rs PNAD_painel_6_rs PNAD_painel_7_rs PNAD_painel_8_rs PNAD_painel_9_rs ///
    PNAD_painel_10_rs PNAD_painel_11_rs PNAD_painel_12_rs



* Montando um arquivo único
cd "$ROOT_DATA"

clear

append using `files'

tempfile base_completa

save "$ROOT/build/output/painel/`base_completa'", replace

* Loop que  cria as bases

local ano1 2012 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021 2021 2022 2022 2022 2022 2023 2023 2023 2023 2024 2024 2024

local tri1 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3

local ano2 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021 2021 2022 2022 2022 2022 2023 2023 2023 2023 2024 2024 2024 2024

local tri2 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4


forvalues i = 1/`: word count `ano1''{

	local ano_inic: word `i' of `ano1'
	local tri_inic: word `i' of `tri1'
	local ano_fin: word `i' of `ano2'
	local tri_fin: word `i' of `tri2'
	
	use `base_completa' if (Ano == `ano_inic' & Trimestre == `tri_inic') | (Ano == `ano_fin' & Trimestre == `tri_fin'), clear
			
	save "$ROOT/build/output/painel_`ano_inic'_`tri_inic'", replace
}

