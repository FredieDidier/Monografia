
## Achando o peso de cada trimestre e consequentemente a sua soma total

df_peso = data_2019 %>%
  group_by(year_quarter) %>%
  summarise(weights = sum(weights))

df_peso = df_peso[-4,]

### Matriz de Transi??o Geral ###

df = data_2019 %>%
  group_by(id_code) %>%
  mutate(id = n()) %>%
  filter(id == 4) %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                         worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                         worker == 1 & work_category == 1 & job_function == 3 ~ 3,
                         worker == 1 & work_category == 2 & job_function == 3 ~ 4,
                         worker == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5,
                         worker == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6,
                         worker == 1 & job_function == 5 & cnpj == 1 ~ 7,
                         worker == 1 & job_function == 5 & cnpj == 2 ~ 8,
                         worker == 1 & job_function == 4 & work_category %in% c(5,7) ~ 9,
                         worker == 1 & job_function == 4 & work_category == 6 ~ 10)) %>%
  filter(!is.na(position)) %>%
  group_by(id_code) %>%
  mutate(new_id = n()) %>%
  filter(new_id == 4) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card, cnpj,
         job_function, occupation_condition, position, social_security_taxpayer, id, new_id,
         work_category)

funcao = function(df, initial_quarter, final_quarter, initial_position,
                  final_position){
  
  df_aux = df %>%
    filter(year_quarter == initial_quarter &
             position == initial_position)
  
  
  initial_number = nrow(df_aux)
  
  initial_list = df_aux$id_code
  
  df_2 = df %>%
    filter(year_quarter == final_quarter &
             position == final_position)
  
  df_2 = df_2 %>%
    filter(id_code %in% initial_list)
  
  final_number = nrow(df_2)
  
  final_number/initial_number
}

 funcao(df, "2019_1", "2019_2", 1, 1)

####

cria_matriz_transicao = function(df, initial_quarter, final_quarter){
  
  matriz = matrix(nrow = 10, ncol = 10)
  
  for(i in 1:10){
  for(j in 1:10){
    
  matriz[i,j] = funcao(df = df,initial_quarter = initial_quarter,
                       final_quarter = final_quarter, initial_position = i,
                       final_position = j)
  
  }
  }
  
  matriz
}
 
matriz1 = cria_matriz_transicao(df, "2019_1", "2019_2")
matriz2 = cria_matriz_transicao(df, "2019_2", "2019_3")
matriz3 = cria_matriz_transicao(df, "2019_3", "2019_4")
matriz_final = (matriz1*df_peso$weights[1] + matriz2 *df_peso$weights[2]
                + matriz3 * df_peso$weights[3])/(sum(df_peso$weights))


# Matriz de Transi??o por Educa??o (Sem educ e fund incompleto)

df = data_2019 %>%
  group_by(id_code) %>%
  mutate(id = n()) %>%
  filter(id == 4) %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                              worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                              worker == 1 & work_category == 1 & job_function == 3 ~ 3,
                              worker == 1 & work_category == 2 & job_function == 3 ~ 4,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6,
                              worker == 1 & job_function == 5 & cnpj == 1 ~ 7,
                              worker == 1 & job_function == 5 & cnpj == 2 ~ 8,
                              worker == 1 & job_function == 4 & work_category %in% c(5,7) ~ 9,
                              worker == 1 & job_function == 4 & work_category == 6 ~ 10)) %>%
  filter(!is.na(position)) %>%
  group_by(id_code) %>%
  mutate(new_id = n()) %>%
  filter(new_id == 4) %>%
  filter(higher_educ_level %in% c(1,2)) %>%
  group_by(id_code) %>%
  mutate(nova_id = n()) %>%
  filter(nova_id == 4) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card, cnpj,
         job_function, occupation_condition, position, social_security_taxpayer, nova_id,
         higher_educ_level, work_category)


matriz4 = cria_matriz_transicao(df, "2019_1", "2019_2")
matriz5 = cria_matriz_transicao(df, "2019_2", "2019_3")
matriz6 = cria_matriz_transicao(df, "2019_3", "2019_4")
matriz_final2 = (matriz4*df_peso$weights[1] + matriz5 *df_peso$weights[2]
                 + matriz6 * df_peso$weights[3])/(sum(df_peso$weights))



# Matriz de Transi??o por Educa??o (fund completo e ensino m?dio incompleto)

df = data_2019 %>%
  group_by(id_code) %>%
  mutate(id = n()) %>%
  filter(id == 4) %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                              worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                              worker == 1 & work_category == 1 & job_function == 3 ~ 3,
                              worker == 1 & work_category == 2 & job_function == 3 ~ 4,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6,
                              worker == 1 & job_function == 5 & cnpj == 1 ~ 7,
                              worker == 1 & job_function == 5 & cnpj == 2 ~ 8,
                              worker == 1 & job_function == 4 & work_category %in% c(5,7) == 1 ~ 9,
                              worker == 1 & job_function == 4 & work_category == 6 ~ 10)) %>%
  filter(!is.na(position)) %>%
  group_by(id_code) %>%
  mutate(new_id = n()) %>%
  filter(new_id == 4) %>%
  filter(higher_educ_level %in% c(3,4)) %>%
  group_by(id_code) %>%
  mutate(nova_id = n()) %>%
  filter(nova_id == 4) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card, cnpj,
         job_function, occupation_condition, position, social_security_taxpayer, nova_id,
         higher_educ_level, work_category)


matriz7 = cria_matriz_transicao(df, "2019_1", "2019_2")
matriz8 = cria_matriz_transicao(df, "2019_2", "2019_3")
matriz9 = cria_matriz_transicao(df, "2019_3", "2019_4")
matriz_final3 = (matriz7*df_peso$weights[1] + matriz8 *df_peso$weights[2]
                 + matriz9 * df_peso$weights[3])/(sum(df_peso$weights))


# Matriz de transi??o por educa??o (ensino m?dio completo e superior incompleto)

df = data_2019 %>%
  group_by(id_code) %>%
  mutate(id = n()) %>%
  filter(id == 4) %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                              worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                              worker == 1 & work_category == 1 & job_function == 3 ~ 3,
                              worker == 1 & work_category == 2 & job_function == 3 ~ 4,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6,
                              worker == 1 & job_function == 5 & cnpj == 1 ~ 7,
                              worker == 1 & job_function == 5 & cnpj == 2 ~ 8,
                              worker == 1 & job_function == 4 & work_category %in% c(5,7) ~ 9,
                              worker == 1 & job_function == 4 & work_category == 6 ~ 10)) %>%
  filter(!is.na(position)) %>%
  group_by(id_code) %>%
  mutate(new_id = n()) %>%
  filter(new_id == 4) %>%
  filter(higher_educ_level %in% c(5,6)) %>%
  group_by(id_code) %>%
  mutate(nova_id = n()) %>%
  filter(nova_id == 4) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card, cnpj,
         job_function, occupation_condition, position, social_security_taxpayer, nova_id,
         higher_educ_level, work_category)


matriz10 = cria_matriz_transicao(df, "2019_1", "2019_2")
matriz11 = cria_matriz_transicao(df, "2019_2", "2019_3")
matriz12 = cria_matriz_transicao(df, "2019_3", "2019_4")
matriz_final4 = (matriz10*df_peso$weights[1] + matriz11 *df_peso$weights[2]
                 + matriz12 * df_peso$weights[3])/(sum(df_peso$weights))

# Matriz de transi??o por educa??o (Superior completo e mais)

df = data_2019 %>%
  group_by(id_code) %>%
  mutate(id = n()) %>%
  filter(id == 4) %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                              worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                              worker == 1 & work_category == 1 & job_function == 3 ~ 3,
                              worker == 1 & work_category == 2 & job_function == 3 ~ 4,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5,
                              worker == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6,
                              worker == 1 & job_function == 5 & cnpj == 1 ~ 7,
                              worker == 1 & job_function == 5 & cnpj == 2 ~ 8,
                              worker == 1 & job_function == 4 & work_category %in% c(5,7) ~ 9,
                              worker == 1 & job_function == 4 & work_category == 6 ~ 10)) %>%
  filter(!is.na(position)) %>%
  group_by(id_code) %>%
  mutate(new_id = n()) %>%
  filter(new_id == 4) %>%
  filter(higher_educ_level %in% c(7)) %>%
  group_by(id_code) %>%
  mutate(nova_id = n()) %>%
  filter(nova_id == 4) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card, cnpj,
         job_function, occupation_condition, position, social_security_taxpayer, nova_id,
         higher_educ_level, work_category)


matriz13 = cria_matriz_transicao(df, "2019_1", "2019_2")
matriz14 = cria_matriz_transicao(df, "2019_2", "2019_3")
matriz15 = cria_matriz_transicao(df, "2019_3", "2019_4")
matriz_final5 = (matriz13*df_peso$weights[1] + matriz14 *df_peso$weights[2]
                 + matriz15 * df_peso$weights[3])/(sum(df_peso$weights))
