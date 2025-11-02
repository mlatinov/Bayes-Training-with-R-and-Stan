
#### Function to make a BRMS Model Height ~ Weight + Age ####
b_model_wa <- function(data){

  #### Libraries ####
  library(brms)
  library(bayesplot)
  library(posterior)
  library(patchwork)

  #### Formula ####
  formula <- brmsformula(height ~ weight + age, family = gaussian)

  #### Specify priors ####
  priors <- c(
    prior(normal(0,5),class = "b",coef = "age"),
    prior(normal(0,5),class = "b",coef = "weight"),
    prior(student_t(5, 135, 10),class = "Intercept"),
    prior(student_t(5, 0, 10), class = "sigma")
  )

  #### Prior Predictive Simulation ####
  prior_sim <- brm(
    formula = formula,
    data = data,
    family = gaussian,
    prior = priors,
    sample_prior = "only",
    chains = 4,
    iter = 2000,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )

  # Check Plausibility of predictions (Prior Predictive Distribution)
  pp_sim <- pp_check(prior_sim, type = "dens_overlay")+
    ggtitle("Prior Predictive Distribution")+
    theme_minimal()

  # Collect in a list
  prior_simulation <- list(
    model = prior_sim,
    prior_predictive_distribution = pp_sim
    )

  #### BRSM Model ####
  model_fit <- brm(
    formula = formula,
    data = data,
    family = gaussian,
    prior = priors,
    chains = 4,
    iter = 2000,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )

  #### Model Specifications  ####

  # Extract posterior draws
  post_draws <- as_draws(model_fit)

  # Collect model specification in a list
  model_specification <- list(
    model = model_fit,
    posterior_draws = post_draws,
    model_summary = summary(model),
    formula = formula,
    priors = priors
  )

  #### CONVERGENCE DIAGNOSTICS ####

  # Trace Plots
  trace_plots <- mcmc_plot(model_fit,type = "trace") +
    theme_minimal()+
    ggtitle("Chains Trace Plots")

  # Autocorrection of the Chains
  auto_chain <- mcmc_acf(model_fit)+
    theme_minimal() +
    ggtitle("Chain Autocorrection Plot")

  # Effective Sample Size Ration
  ef_sample <- mcmc_neff(neff_ratio(model_fit))+
    theme_minimal() +
    ggtitle("Effective Sample Size Ration")

  # Collect Convergent Diagnostics in a list
  convergence_diagnostics <- list(
    trace_plot = trace_plots,
    autocorrelation_chain  = auto_chain,
    effective_sample_ration = ef_sample
  )

  #### PRIOR Sensitivity Analysis ####









}
