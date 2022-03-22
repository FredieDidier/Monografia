cd C:\GitHub\Monografia

* Gerando painel

datazoom_pnadcontinua, years(2012) ///
					original("C:\GitHub\Monografia\Raw Data") ///
					saving("C:\GitHub\Monografia\Raw Data\Painel") ///
					idrs
					
* Gerando arquivos separados por pares de trimestres

cd C:\GitHub\Monografia

local files pnad_painel_1_rs pnad_painel_2_rs pnad_painel_3_rs pnad_painel_4_rs ///
		pnad_painel_5_rs pnad_painel_6_rs pnad_painel_7_rs pnad_painel_8_rs

* Montando um arquivo único
cd "C:\GitHub\Monografia\Raw Data\Painel\pnadcontinua"

clear

append using `files'

tempfile base_completa

save `base_completa', replace

* Loop que  cria as bases

local ano1 2012 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021
local tri1 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3
local ano2 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021 2021
local tri2 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 

forvalues i = 1/`: word count `ano1''{

	local ano_inic: word `i' of `ano1'
	local tri_inic: word `i' of `tri1'
	local ano_fin: word `i' of `ano2'
	local tri_fin: word `i' of `tri2'
	
	use `base_completa' if (Ano == `ano_inic' & Trimestre == `tri_inic') | (Ano == `ano_fin' & Trimestre == `tri_fin'), clear
			
	save "C:\GitHub\Monografia\Raw Data\Trimestres\painel_`ano_inic'_`tri_inic'", replace
}