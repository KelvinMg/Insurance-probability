library(dslabs)
library(dplyr)
data(death_prob)
head(death_prob)

#a function that checks probability of any particular age 
probability_checker <- function(x, y) {
  prob_age_sex <- death_prob %>% 
    filter(age == x & sex == as.character(y)) %>%
    .$prob
  print(prob_age_sex)
  
}

#takes age and sex
analysis_any_age<- function(x, y){
  
  cat("this analysis is for a", y, "aged", x, "\n")
  
  prob_age_sex <- death_prob %>% 
    filter(age == x & sex == as.character(y)) %>%
    .$prob
  cat("the probability of death is ", prob_age_sex, "\n")
  
  a <- -150000
  b <- 1150
  
  expected_value <- (prob_age_sex * a) + ((1 - prob_age_sex) * b)
  cat("the expected profit or loss is", 
      paste0(" $", expected_value), "\n")
  
  standard_error <- abs(b-a) * sqrt(prob_age_sex * (1 - prob_age_sex))
  cat("it is expected to vary by", paste0("$", standard_error), "\n")
  
  expected_value_1000_policies <- 1000 * expected_value
  cat("the expected profit or loss for 1000 policies is", 
      paste0("$", expected_value_1000_policies), "\n")
  
  standard_error_1000_policies <- sqrt(1000) * standard_error
  cat("it is expected to vary by", 
      paste0("$", standard_error_1000_policies), "\n")
  
  prob_age_sex_1000_loss <- pnorm(0, expected_value_1000_policies, 
                                  standard_error_1000_policies)
  cat("the probability for making a loss on 1000 policies is", prob_age_sex_1000_loss)
}

#the function takes variables x and n with x being the age and n the sex
analysis_any_age(40, "Female")


