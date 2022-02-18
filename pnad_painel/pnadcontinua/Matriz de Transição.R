

df = data_2019 %>%
  group_by(id_code) %>%
  mutate(id = n()) %>%
  filter(id == 4) %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                         worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                         worker == 1 & signed_work_card == 2 ~ 3,
                         worker == 1 & signed_work_card == 1 ~ 4,
                         worker == 1 & job_function == 6 ~ 5,
                         worker == 1 & job_function == 5 ~ 6,
                         worker == 1 & job_function == 4 ~ 7)) %>%
  filter(!is.na(position)) %>%
  group_by(id_code) %>%
  mutate(new_id = n()) %>%
  filter(new_id == 4) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card,
         job_function, occupation_condition, position, id, new_id)

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

 funcao(df, "2019_3", "2019_4", 1, 1)

####

cria_matriz_transicao = function(df, initial_quarter, final_quarter){
  
  matriz = matrix(nrow = 7, ncol = 7)
  
  for(i in 1:7){
  for(j in 1:7){
    
  matriz[i,j] = funcao(df = df,initial_quarter = initial_quarter,
                       final_quarter = final_quarter, initial_position = i,
                       final_position = j)
  matriz
  
  }
  }
}
 
matriz1 = cria_matriz_transicao("2019_1", "2019_2")


######### Linhas/Colunas da Matriz ####
# Linha 1: 63%, 3,4%, 1,8%, 1,3%, 2,3%, 0,1%, 0,07%
# Linha 2: 19,3%, 29%, 7,3%, 5%, 5,3%, 0,1%, 0,13%
# Linha 3: 7%, 3,9%, 42%, 7%, 6,3%, 0,63%, 1,5%
# Linha 4: 2%, 1,7%, 3%, 61%, 1,2%, 0,3%, 1%
# Linha 5: 7,3%, 2%, 4,8%, 1,6%, 52%, 2,2%, 0,1%
# Linha 6: 2,2%, 0,4%, 2,8%, 2,2%, 14%, 50%, 0,2%
# Linha 7: 1,2%, 0,18%, 3,8%, 4,5%, 0,4%, 0,12%, 61%   

#################
### Diagonais da Matriz ####
# 1. (0.63, 0.63, 0,63 = 63%)
# 2.(0.30, 0,32, 0,31 = 31%)
# 3.(0,42, 0,43, 0,42 = 42%)
# 4.(0.61, 0,61, 0,61 = 61%)
# 5.(0,52, 0,52, 0,52 = 52%)
# 6.(0.50, 0.50, 0.51 = 50%)
# 7.(0,62, 0,61, 0,61 = 61%)