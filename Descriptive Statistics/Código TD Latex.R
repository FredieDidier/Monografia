 ## Esse script foi criado apenas para capturar o códido da tabela descritiva em
## formato latex. Pode esquecer essas variáveis abaixo (já que alteramos o formato
## da TD). O script que usei com as variáveis para a tabela descritiva é o
## "Tabela Descritiva"

library(modelsummary)

Mean <- function(x) mean(x, na.rm = TRUE)
Sd = function(x) sd(x, na.rm = TRUE)

datasummary(monthly_work_income + years_of_study + age + Male + Female +
              signed_card_employee + no_signed_card_employee +
              workforce + no_workforce + occupation + no_occupation +
              social_security_contributor + 
              no_social_security_contributor + social_security_and_self_employed +
              non_social_security_and_self_employed +
              hours_worked
              ~ Mean + Sd,
            data = data_2019, output = "latex")
