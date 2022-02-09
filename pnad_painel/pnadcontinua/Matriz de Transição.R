

df = data_2019 %>%
  mutate(position = case_when(workforce_condition == 2 ~ 1,
                         worker == 2 ~ 2,
                         worker == 1 & signed_work_card == 2 ~ 3,
                         worker == 1 & signed_work_card == 1 ~ 4,
                         worker == 1 & job_function == 6 ~ 5,
                         worker == 1 & job_function == 5 ~ 6,
                         worker == 1 & job_function == 4 ~ 7)) %>%
  select(id_code, year_quarter, workforce_condition, worker, signed_work_card,
         job_function, position)

funcao = function(df, initial_quarter, final_quarter, initial_position,
                  final_position){
  
  df_aux = df %>%
    filter(year_quarter == initial_quarter &
             position == initial_position)
  
  initial_number = nrow(df_aux)
  
  initial_list = df_aux$id_code
  
  df_2 = df %>%
    filter(year_quarter == final_quarter &
             position == final_position & id_code %in% initial_list)
  
  final_number = nrow(df_2)
  
  final_number/initial_number
}

funcao(df, "2019_3", "2019_4", 7, 7)


################
### Diagonais da Matriz ####
# 1. (0.63, 0.63, 0,63 = 63%)
# 2.(0.30, 0,32, 0,31 = 31%)
# 3.(0,42, 0,43, 0,42 = 42%)
# 4.(0.61, 0,61, 0,61 = 61%)
# 5.(0,52, 0,52, 0,52 = 52%)
# 6.(0.50, 0.50, 0.51 = 50%)
# 7.(0,62, 0,61, 0,61 = 61%)