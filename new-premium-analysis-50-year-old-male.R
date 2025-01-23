#probability of death of a 50 year old male 
prob_50_Male <- probability_checker(50, "Male") 

#suppose the company wants its expected profits 1000 50 year olds
#with 150000 life insurance contracts to be 700000 what premium 
#should they charge the formula is (700000/n - a*p) / 1-p
new_premium <- (700000/1000 - (a*prob_50_Male))/(1 - prob_50_Male)
new_premium

#the standard error for 1000 policies is 
a <- -150000
b <- new_premium

standard_error <- abs(b-a) * sqrt(x * (1 - x))
standard_error_1000_policies <- sqrt(1000) * standard_error
standard_error_1000_policies


#probability of losing money
pnorm(0, 700000, standard_error_1000_policies)
