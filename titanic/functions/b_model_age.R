##### BRMS Model for estimating the direct effect of age on the survival ####

b_model_age <- function(data){

  # Bayes Model
  model <- brm(
    formula = survived ~ 1 + age + class + sex,
    data = data,
    family = bernoulli,
    prior = c(
              prior(normal(0, 1), class = "Intercept"),
              prior(normal(0, 1), class = "b")
              ),
    iter = 2000,
    warmup = 500,
    chains = 4,
    seed = 111
    )
  # Inspecting Model Fit and Convergence
  model_summary <- summary(model)
  fixed_model <- fixef(model)
  posterior_predictive <- pp_check(model, type = "dens_overlay", binwidth = 0.1)
  chain_trace <- plot(model,type = "trace")

  # Calculate LOO
  loo_result <- loo(model)

  # Posterior distribution
  post <- as_draws_df(model)
}

