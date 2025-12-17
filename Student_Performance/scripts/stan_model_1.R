
#### Function to compile Stan Model 1 ####
stan_model_1 <- function(data,file){

  # Convert the dataframe to list for Stan
  stan_data <- list(
    N = nrow(data),
    X = data$previous_score,
    Y = data$test_result
  )

  # Model
  model <- cmdstan_model(stan_file = file )

  # Fit the model
  fit_model <- model$sample(
    data = stan_data,
    output_dir = "stan_output/",
    save_cmdstan_config = TRUE,
    seed = 123
    )

  # Return the model
  return(fit_model)
}
