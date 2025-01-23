#the adding the values to a new dataset containing new variables

head(death_prob)

death_prob_analysed <- death_prob %>%
  mutate(expected_value = (prob * a) + ((1 - prob) * b),
  
  standard_error = abs(b-a) * sqrt(prob * (1 - prob)),
 
  expected_value_1000_policies = 1000 * expected_value,
 
  standard_error_1000_policies = sqrt(1000) * standard_error,
  
  prob_age_sex_1000_loss = pnorm(0, expected_value_1000_policies, 
                                  standard_error_1000_policies)
)

head(death_prob_analysed)
View(death_prob_analysed)

