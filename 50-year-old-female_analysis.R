library(dslabs)
library(dplyr)
data(death_prob)
head(death_prob)
#probability of a female 50 year old female
prob_50_fem <- death_prob %>% 
  filter(age == "50" & sex == "Female") %>%
  .$prob

cat("The probability is ", prob_50_fem)

#expected profit or loss on one policy for a 50 year old female
#if the female dies the company losses -$150,000 and if she lives
#the company gain $1,150
#the formula is ap + b(1-p)
a <- -150000
b <- 1150
expected_value <- (prob_50_fem * a) + ((1 - prob_50_fem) * b)
cat("The expected profit or loss is", paste0("$", expected_value))

#The standard error of the expected value for one policy is 
#the formula is |b-a| * sqrt(p*(1-p))
standard_error <- abs(b-a) * sqrt(prob_50_fem * (1 - prob_50_fem))
cat("It is expected to vary by", paste0("$", standard_error))

#for a thousand policies the expected profit or loss is
#the formula is n (ap + b(1-p))
expected_value_1000_policies <- 1000 * expected_value
cat("the expected profit or loss for 1000 policies is ", paste0("$", expected_value_1000_policies))

#The standard error of the expected value for 1000 policies is
#the formula is sqrt(n) * standard error of one policy
standard_error_1000_policies <- sqrt(1000) * standard_error
cat("It is expected to vary by", paste0("$", standard_error_1000_policies))

#to calculate the probability of the company losing money on the 1000 policies
#we use the central limit theorem pnorm using the expected value and standard
#error for the 1000 policies
prob_50_fem_1000_loss <- pnorm(0, expected_value_1000_policies, 
                               standard_error_1000_policies)


cat("the probability for making a loss for 1000 policies of 50 year old female 
    policies is ", prob_50_fem_1000_loss)


