# one function to calculate new premiums from new expected profits for any age and
# sex.
#second function to get an analysis using the newly calculated premium
#global variables with a representing the payout incurred by the company
#if the policy-holder dies
#y representing the no of policies and
#z the expected profits of y policies

a <- -150000
y <- 1000
z <- 700000



#takes probability, no of policies and expected profits and returns new premium
new_premium_function_1000 <- function(x, y, z) {
  new_premium <- (z/y - (a*x))/(1 - x)
  #cat("new premium", new_premium)
  new_premium
}

#getting probability and then using new premium fucntion

x <- probability_checker(50, "Male")
new_premium <-new_premium_function_1000(x, y, z)
new_premium

#takes probability, no of policies, expected profits and new premium
# and then does a new analysis

analysis_new_premium <- function(x, y, z, n){
  
  
  
  
  b <- n
  
  expected_value <- z/y
  cat("the expected profit or loss per person is", 
      paste0(" $", expected_value), "\n")
  
  standard_error <- abs(b-a) * sqrt(x * (1 - x))
  cat("it is expected to vary by", paste0("$", standard_error), "\n")
  
  expected_value_y_policies <- z
  cat("the expected profit or loss for y policies is", 
      paste0("$", expected_value_y_policies), "\n")
  
  standard_error_y_policies <- sqrt(y) * standard_error
  cat("it is expected to vary by", 
      paste0("$", standard_error_y_policies), "\n")
  
  prob_age_sex_y_loss <- pnorm(0, expected_value_y_policies, 
                               standard_error_y_policies)
  cat("the probability for making a loss on y policies is", prob_age_sex_y_loss)
}

#takes probability, new premium and the expected profits of 1000 policies
#that was used to calculate the new premium and does an analysis
analysis_new_premium(x, y, z, new_premium)