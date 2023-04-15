# This code defines a function called funcao that takes as input a data frame (df), four strings (initial_quarter, final_quarter, initial_position, final_position), and a logical value (prop).
# 
# The function starts by subsetting the input data frame df to only include observations that have a year_quarter value equal to either the initial_quarter or final_quarter inputs, and that also have a non-missing position value.
# 
# It then creates a new variable called new_id that counts the number of observations for each unique id_code in the data frame. It selects only the observations where new_id equals 2, which correspond to the second quarter in the sequence.
# 
# It then creates a new data frame called df_aux that selects only the observations where the year_quarter equals initial_quarter and position equals initial_position.
# 
# It then calculates the initial_number by summing the weights variable in df_aux.
# 
# It creates a new list called initial_list that includes the id_code values from df_aux.
# 
# It then creates a new data frame called df_2 that first groups the data by id_code and creates a new variable called weights that selects the first weights value based on the year_quarter order within each id_code group. It then subsets the data frame to only include observations where year_quarter equals final_quarter and position equals final_position.
# 
# It then subsets df_2 to only include observations where the id_code is in initial_list.
# 
# Finally, the function calculates the final_number by summing the weights variable in df_2.
# 
# If prop is TRUE, the function returns the ratio of final_number to initial_number. If prop is FALSE, the function returns final_number.




funcao = function(df, initial_quarter, final_quarter, initial_position,
                  final_position, prop = TRUE){
  
  df <- df[year_quarter %in% c(initial_quarter, final_quarter) & !is.na(position)]
  
  df = df[, new_id := .N, by = id_code][new_id == 2]
  
  df_aux <- df[year_quarter == initial_quarter & position == initial_position]
  
  initial_number = sum(df_aux$weights)
  
  initial_list = df_aux$id_code
  
  df_2 = df[, weights := first(weights, order_by = year_quarter), by = id_code][  year_quarter == final_quarter & position == final_position]
  
  df_2 = df_2 %>%
    filter(id_code %in% initial_list)
  
  final_number = sum(df_2$weights)
  
  if(prop){
    final_number/initial_number
  }
  else{
    final_number
  }
}

####
# df = df %>%
#   filter(year_quarter %in% c(initial_quarter, final_quarter)) %>%
#   filter(!is.na(position)) %>%
#   group_by(id_code) %>%
#   mutate(new_id = n()) %>%
#   filter(new_id == 2) 
# 
# df_aux = df %>%
#   filter(year_quarter == initial_quarter &
#            position == initial_position)
# 
# 
# initial_number = sum(df_aux$weights)
# 
# initial_list = df_aux$id_code
# 
# df_2 = df %>%
#   group_by(id_code) %>%
#   mutate(weights = first(weights, order_by = year_quarter)) %>%
#   filter(year_quarter == final_quarter &
#            position == final_position)
# 
# df_2 = df_2 %>%
#   filter(id_code %in% initial_list)

####

cria_matriz_transicao = function(df, initial_quarter, final_quarter, n, prop = TRUE){
  
  matriz = matrix(nrow = n, ncol = n)
  
  for(i in 1:n){
    for(j in 1:n){
      
      matriz[i,j] = funcao(df = df,initial_quarter = initial_quarter,
                           final_quarter = final_quarter, initial_position = i,
                           final_position = j, prop = prop)
      
    }
  }
  
  matriz
}
