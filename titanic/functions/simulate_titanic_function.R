
#### Function to simulate Titanic ####
simulate_titanic <- function(n){

# Simulate Sex and Age from a Binomial Distribution
sex <- rbinom(n,1,0.5)    # 1 = Female, 0 = Male
age <- rbinom(n, 1, 0.1)  # 1 = Child,  0 = Adult

# Simulate Class with Causal relationship with Age and Sex

# Calculate the linear predictors for each class vs 4th
eta_1 <- -1.39 + 1.2 * age +  0.9 *  sex
eta_2 <- -0.98 + 0.8 * age +  0.6 *  sex
eta_3 <- -0.13 - 0.9 * age + -0.1 *  sex

# Covert to probability using softmax
prob_1 <- exp(eta_1)
prob_2 <- exp(eta_2)
prob_3 <- exp(eta_3)
prob_4 <- 1

# Normalize to get probabilities that sum to 1
total <- prob_1 + prob_2 + prob_3 + prob_4
p_matrix <- cbind(prob_1,prob_2,prob_3,prob_4) / total

# Sample class for each person
class <- numeric(n)
for(i in 1:n) {
  class[i] <- sample(1:4, 1, prob = p_matrix[i,])
}

# Calculate survival probability for each person
eta_survival <- -1.4 +
  1.2 * (class == 1) +    # 1st class
  0.8 * (class == 2) +    # 2nd class
  0.6 * (class == 3) +    # 3rd class
  1.2 * (sex == 1) +      # female
  1.3 * (age == 1)        # child

# Convert log-odds to probability
p_survival <- plogis(eta_survival)

# Simulate survival
survived <- rbinom(n, 1, p_survival)

# Return a datafrane
return(
  data.frame(
  sex,
  age,
  class,
  survived
))
}
