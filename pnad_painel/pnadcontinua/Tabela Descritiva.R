 ## Fazendo Tabela Descritiva

library(modelsummary)

Mean <- function(x) mean(x, na.rm = TRUE)
Sd = function(x) sd(x, na.rm = TRUE)

datasummary(monthly_work_income + years_of_study + age + Male + Female +
              signed_card_employee + no_signed_card_employee +
              last_week_worker + no_last_week_worker + 
              social_security_contributor + 
              no_social_security_contributor + hours_worked +
              ~ Mean + Sd,
            data = data_2019)
