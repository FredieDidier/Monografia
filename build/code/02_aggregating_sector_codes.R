# The following function takes a data frame as input and modifies it by adding a column called "sector_code". The column is assigned a new value depending on the value of the existing "sector_code" column. The new value is a text description of the economic sector associated with the numeric code. Here are some comments on the specific sectors that are defined:
  
## Agriculture, Livestock, Forestry and Fishing: codes between 1101 and 1119, 1201 and 1209, 1401 and 1402, 1500, 2000, 3001 and 3002.
## Extractive industries: codes 5000, 6000, 7001, 7002, 8001, 8002, 8009, and 9000.
## Manufacturing industries: numerous codes.
## Electricity and Gas: codes 35010, 35021, and 35022.
## Water, Sewage, Waste Management, and Decontamination: codes 36000, 37000, 38000, and 39000.
## Construction: codes 41000, 42000, and 43000.
## Commerce and repair of motor vehicles: codes 45010, 45020, 45030, and 45040.
## Transport, Warehousing, and Mail: numerous codes.
## Accommodation and Food: numerous codes.
## Information and Communication: numerous codes.
## Financial and Insurance Activities: codes 64000, 65000, 66001, and 66002.
## Real Estate Activities: code 68000.
## Professional, Scientific, and Technical Activities: numerous codes.
## Administrative and Complementary Services: numerous codes.
## Public Administration, Defense, and Social Security: numerous codes.
## Education: numerous codes.
## Human Health and Social Services: numerous codes.
## Arts, Culture, Sports, and Recreation: numerous codes.
## Other Services: numerous codes.

aggregate_sectors = function(df){

  df = df %>%
    mutate(sector_code = case_when(sector_code %in% c(1101, 1102,
                                                    1103, 1104,
                                                    1105, 1106,
                                                    1107, 1108, 1109,
                                                    1110, 1111, 1112,
                                                    1113, 1114, 1115,
                                                    1116, 1117, 1118, 1119, 
                                 1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208, 1209,
                                 1401, 1402, 1500, 2000, 3001, 3002) ~ "Agricultura, Pecuaria, Producao Florestal e Pesca",
                                 sector_code %in% c(5000, 6000,7001, 7002, 8001, 8002, 8009, 9000) ~ "Industrias Extrativas",
                                 sector_code %in% c(10010, 10021, 10022, 10030, 10091, 10092, 10093, 10099,
                                                    11000, 12000, 13001, 13002, 14001, 14002, 15011, 15012, 15020,
                                                    16001, 16002, 17001, 17002, 18000, 19010, 19020, 19030,
                                                    20010, 20020, 20090, 21000, 22010, 22020,
                                                    23010, 23091, 23099, 24001, 24002, 24003,
                                                    25001, 25002, 26010, 26020, 26030, 26041, 26042,
                                                    27010, 27090, 28000, 29001, 29002, 29003, 30010, 30020, 30030, 300090,
                                                    31000,32001, 32002, 32003, 32009, 33001, 33002) ~ "Industrias de Transformacao",
                                 sector_code %in% c(35010, 35021, 35022) ~ "Eletricidade e Gas",
                                 sector_code %in% c(36000, 37000, 38000, 39000) ~ "Agua, Esgoto, Gestao de Residuos e Descontaminacao",
                                 sector_code %in% c(41000, 42000, 43000) ~ "Construcao",
                                 sector_code %in% c(45010, 45020, 45030, 45040) ~ "Comercio e Reparacao de Veiculos Automotores",
                                 sector_code %in% c(49010, 4030, 49040, 49090, 50000, 51000, 52010, 52020, 53001, 53002) ~ "Transporte, Armazenagem e Correio",
                                 sector_code %in% c(55000, 56011, 56012, 56020) ~ "Alojamento e Alimentacao",
                                 sector_code %in% c(58000, 59000, 60001, 60002, 61000, 62000, 63000) ~ "Informacao e Comunicacao",
                                 sector_code %in% c(64000, 65000, 66001, 66002) ~ "Atividades Financeiras e de Seguros",
                                 sector_code == 68000 ~ "Atividades Imobiliarias",
                                 sector_code %in% c(69000, 70000, 71000, 72000, 73010, 73020, 74000, 75000) ~ "Atividades Profissionais, Cientificas e Tecnicas",
                                 sector_code %in% c(77010, 77020, 78000, 79000, 80000, 81011, 81012, 81020, 82001, 82002, 82003, 82009) ~ "Atividades Administrativas e Servicos Complementares",
                                 sector_code %in% c(84011, 84012, 84013, 84014, 84015, 84016, 84017, 84020) ~ "Administracao Publica, Defesa e Seguridade Social",
                                 sector_code %in% c(85011, 85012, 85013, 85014, 85021, 85029) ~ "Educacao",
                                 sector_code %in% c(86001, 86002, 86003, 86004, 86009, 87000, 88000) ~ "Saude Humana e Servicos Sociais",
                                 sector_code %in% c(90000, 91000, 92000, 93011, 93012, 93020) ~ "Artes, Cultura, Esportes e Recreacao",
                                 sector_code %in% c(94010, 94020, 94091, 94099, 95010, 95030, 96010, 96020, 96030, 96090) ~ "Outras Atividades de Servicos",
                                 sector_code == 97000 ~ "Servicos Domesticos",
                                 sector_code == 99000 ~ "Organismos Internacionais",
                                 sector_code == 0 ~ "Atividades Mal Definidas",
                                 sector_code == 1999 ~ "Agropecuaria",
                                 sector_code %in% c(48010, 48020, 48030, 48041, 48042, 48050, 48060,
                                                    48071, 48072, 48073, 48074, 48075, 48076, 48077, 48078,
                                                    48079, 48080, 48090, 48100) ~ "Comercio, exceto de Veiculos Automotores e Motocicletas"))
  df
}
