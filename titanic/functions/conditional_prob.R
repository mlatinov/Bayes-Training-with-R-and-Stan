
#### Function to Calculate conditional prob from log odds scale ####
conditional_prob <- function(
    dataset,
    post_draws,
    age = c("0","1"),
    sex = c("0","1"),
    class = c("1","2","3","4")
    ){

  # Take the intercept
  log_odds <- post_draws$b_Intercept

  # Add age effect if is not ref level
  if (age != levels(as.factor(dataset$age))[1]) {
    log_odds <- log_odds + post_draws$b_age1
  }
  # Add sex effect if is not a ref level
  if (sex != levels(as.factor(dataset$sex))[1]) {
    log_odds <- log_odds + post_draws$b_sex1
  }
  # Add class if is not a ref level
  if(class == "2") {
    log_odds <- log_odds + post_draws$b_class2
  } else if(class == "3") {
    log_odds <- log_odds + post_draws$b_class3
  } else if(class == "4") {
    log_odds <- log_odds + post_draws$b_class4
  }

  # Convert to probability
  probability <- exp(log_odds) / (1 + exp(log_odds))

  return(probability)
}
