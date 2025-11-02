
#### Function to make a Generative model for Howell dataset ####
howell_make_gen <- function(n){

  #### Libraries ####
  library(splines)

  # Covariates
  sex <- rbinom(n, 1, 0.5)
  age <- pmax(round(rnorm(n, mean = 20, sd = 10)), 1)

  # Noise terms
  eps_w <- rnorm(n, 0, 2.5)   # noise for weight
  eps_h <- rnorm(n, 0, 3.0)   # noise for height

  # Build spline basis
  df_basis <- 4
  B_age_w <- ns(age, df = df_basis)      # basis for age -> weight
  B_age_h <- ns(age, df = df_basis)      # basis for age -> height

  # Define target smooth shapes
  f_age_to_weight <- function(a){
    5 + 0.9 * a - 0.02 * a^2 + 2.5 * sin(a / 6)
  }

  target_w_age <- f_age_to_weight(age)

  # Project target shape onto the spline basis to get coefficients
  coef_w_age <- coef(lm(target_w_age ~ B_age_w - 1))

  # mu_weight intercept and sex effect
  Intercept_w <- 50    # baseline weight
  sex_effect_w <- 10   # added kg if sex == 1

  mu_weight <- Intercept_w + as.vector(B_age_w %*% coef_w_age) + sex_effect_w * sex
  weight <- mu_weight + eps_w

  # Target for height:
  f_age_to_height <- function(a){
    40 + 4.0 * a - 0.06 * a^2 + 5 * sin(a / 8)
  }

  target_h_age <- f_age_to_height(age)

  # Project age-> height target onto the age
  coef_h_age <- coef(lm(target_h_age ~ B_age_h - 1))

  # Create a basis for weight
  B_wht <- ns(weight, df = df_basis)

  # Define a target function of weight
  f_weight_to_height <- function(w){
    0.7 * w + 0.01 * (w - 45)^2 / 10
  }
  target_h_weight <- f_weight_to_height(weight)
  coef_h_weight <- coef(lm(target_h_weight ~ B_wht - 1))

  # Generative height: intercept + age-spline + weight-spline + sex + noise
  Intercept_h <- 10   # baseline height in cm
  sex_effect_h <- 10  # males taller on average

  mu_height <- Intercept_h +
    as.vector(B_age_h %*% coef_h_age) +
    as.vector(B_wht %*% coef_h_weight) +
    sex_effect_h * sex

  height <- mu_height + eps_h

  # Combine into data frame and scale the predictors
  data_gen <- data.frame(
    age = scale(age,center = TRUE),
    sex = as.factor(sex),
    weight = scale(weight,center = TRUE),
    height = height
    )

  # Return
  list(
    data_generated = data_gen,
    plots = list(
      hist_weight = hist(weight),
      hist_height = hist(height),
      weight_height = plot(x = weight,y = height),
      age_height =  plot(x = age, y = height),
      age_weight = plot(x = age , y = weight)
    )
  )
}
