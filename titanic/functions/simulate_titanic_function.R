
#### Function to simulate Titanic ####
simulate_titanic <- function(n){

# Simulate Sex and Age from a Binomial Distribution
# 0 = Women , 1 = Men
sex <- rbinom(n,1,0.5)

# 0 = Child, 1 = Adult
age <- rbinom(n,1,0.8)

# Simulate Class with Causal relationship with Age and Sex
class_prob <- case_when(
  sex == 1 & age == 1 ~ 0.5, # Adult Men
  sex == 1 & age == 0 ~ 0.5, # Boys
  sex == 0 & age == 1 ~ 0.5, # Adult Woman
  sex == 0 & age == 0 ~ 0.5  # Girl
  )
class <- rbind(n,1,class_prob)

}
