* ------------------------------------------------------------------------

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
	global ROOT "C:\GitHub\Monografia"
}

cd $ROOT

* Gerando painel

datazoom_pnadcontinua, years(2012/2022) ///
					original("$ROOT\data-raw") ///
					saving("$ROOT\data-raw\Painel") ///
					idrs
					
* Gerando arquivos separados por pares de trimestres

cd $ROOT

local files PNAD_painel_1_rs PNAD_painel_2_rs PNAD_painel_3_rs PNAD_painel_4_rs ///
		PNAD_painel_5_rs PNAD_painel_6_rs PNAD_painel_7_rs PNAD_painel_8_rs PNAD_painel_9_rs

* Montando um arquivo único
cd "$ROOT\data-raw\Painel\pnadcontinua"

clear

append using `files'

tempfile base_completa

save `base_completa', replace

* Loop que  cria as bases

local ano1 2012 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021 2021 2022 2022
local tri1 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2
local ano2 2012 2012 2012 2013 2013 2013 2013 2014 2014 2014 2014 2015 2015 2015 2015 2016 2016 2016 2016 2017 2017 2017 2017 2018 2018 2018 2018 2019 2019 2019 2019 2020 2020 2020 2020 2021 2021 2021 2021 2022 2022 2022
local tri2 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3

forvalues i = 1/`: word count `ano1''{

	local ano_inic: word `i' of `ano1'
	local tri_inic: word `i' of `tri1'
	local ano_fin: word `i' of `ano2'
	local tri_fin: word `i' of `tri2'
	
	use `base_completa' if (Ano == `ano_inic' & Trimestre == `tri_inic') | (Ano == `ano_fin' & Trimestre == `tri_fin'), clear
			
	save "C:\GitHub\Monografia\data-raw\Trimestres\painel_`ano_inic'_`tri_inic'", replace
}


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
