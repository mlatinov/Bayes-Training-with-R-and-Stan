
#### Function to make a Generative model for Howell dataset ####
howell_make_gen <- function(n){

  # Simulate sex from a bernulli distribution
  sex <- rbinom(n = 1000,size = 1,prob = 0.5)

  # Simulate Age from a Normal distribution with mean 40 and sd of 10
  age <- pmax(round(rnorm(n = 1000, mean = 20,sd = 10)),1)

  # Simulate weight and the effect of age and sex + random error
  random_error <- rnorm(n = 1000,mean = 0,sd = 2)
  weight <- case_when(
    age <= 9 & age >= 1 ~ 20 + 0.9 * age + 5 * sex + random_error,
    age > 9  & age <= 18 ~ 40 + 0.7 * age + 10 * sex + random_error,
    age > 18 & age < 30 ~ 50 + 0.5 * age + 15 * sex + random_error,
    age >= 30 ~ 50 + 0.4 * age + 15 * sex + random_error
  )

  # Simulate height
  height <- case_when(
    age <= 9 & age >= 1 ~ 85 + 0.8 * age + 0.8 * weight + 5 * sex + random_error,
    age > 9 & age <= 18 ~ 90 + 0.9 * age + 0.9 * weight + 10 * sex + random_error,
    age > 18 & age < 30 ~ 93 + 0.4 * age + 0.8 * weight + 10 * sex + random_error,
    age >= 30 ~ 90 + 0.4 * age + 0.8 * weight + 10 * sex + random_error
  )

  # Combine all in one dataframe
  data <- data.frame(
    sex = sex,
    age = age,
    weight = weight,
    height = height
  )

  # Return
  list(
    data_generated = data,
    plots = list(
      hist_weight = hist(weight),
      hist_height = hist(height),
      weight_height = plot(x = weight,y = height),
      age_height =  plot(x = age, y = height),
      age_weight = plot(x = age , y = weight)
    )
  )
}
