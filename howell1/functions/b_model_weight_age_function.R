
#### Function to make a BRMS Model Height ~ Weight + Age ####
b_model_wa <- function(data){

  #### Libraries ####
  library(brms)
  library(bayesplot)
  library(posterior)
  library(priorsense)
  library(patchwork)

  #### Formula ####
  formula <- brmsformula(height ~ s(weight,k = 5,bs = "cr") + age, family = gaussian)

  #### Specify priors ####
  priors <- c(
    prior(normal(0, 5), class = "b", coef = "age"),         # age effect
    prior(normal(0, 3), class = "b", coef = "sweight_1"),   # spline basis coeffs for weight
    prior(exponential(0.8), class = "sds",
          coef = "s(weight, k = 5, bs = \"cr\")"),          # spline SD
    prior(student_t(5, 135, 10), class = "Intercept"),      # Intercept
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
    iter = 3000,
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
    model_summary = summary(model_fit),
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

  # Numerical Check
  sens_table <- powerscale_sensitivity(model_fit)

  # Posterior density under prior perturbations
  sens_density <-
    powerscale_plot_dens(model_fit, params = c("b_sweight_1", "sds_sweight_1", "b_age"))

  # Cumulative probability under prior perturbations
  sens_ecdf <-
    powerscale_plot_ecdf(model_fit, params = c("b_sweight_1", "sds_sweight_1", "b_age"))

  # Collect in a list
  prior_sensitivity <- list(
    summary_table = sens_table,
    posterior_density = sens_density,
    posterior_ecdf = sens_ecdf
  )

  #### POSTERIOR Summaries ####

  # Conditional effect of weight on height
  ce_weight <- conditional_effects(model_fit, effects = "weight")

  # Plot with custom labels and theme
  ce_weight_plot <- plot(ce_weight, points = TRUE)[[1]] +
    ggtitle("Conditional Effect of Weight on Height") +
    labs(
      x = "Weight",
      y = "Predicted Height (cm)"
    ) +
    theme_minimal()

  # Credible Intervals
  intervals <- mcmc_intervals(model_fit)+
    ggtitle("Credible Intervals")+
    theme_minimal()

  # Table summary
  post_summary <- posterior_summary(model_fit)

  # Collect posterior summary in a list
  posterior_summaries <- list(
    posterior_distributions = posterior_weight,
    credible_interval = intervals,
    posterior_summary = post_summary
  )

  #### Posterior Predictive Checks ####

  # Posterior Predictive Distribution
  pp_dens <- pp_check(model_fit,type = "dens_overlay")+
    ggtitle("Posterior Predictive Distribution")+
    theme_minimal()

  # Posterior Predictive Error
  pp_error <- pp_check(model_fit,type = "error_binned")+
    ggtitle("Posterior Predictive Errors")+
    theme_minimal()

  # Collect pp checks in a list
  posterior_predictive_checks <- list(
    post_pred_density = pp_dens,
    posterior_error = pp_error
    )

  #### MODEL COMPARISON & FIT ####
  loo <- loo(model_fit)

  # Collect in a list
  model_comparison <- list(
    loo_result = loo,
    pareto_k_plot = plot(loo),
    loo_summary = data.frame(
      Metric = c("ELPD", "p_loo", "LOOIC"),
      Value = loo$estimates[, "Estimate"],
      SE = loo$estimates[, "SE"]
    ),
    bayes_r = bayes_R2(model_fit)
  )

  # Return Everything collected
  return(list(
    model = model,
    prior_simulation = prior_simulation,
    convergence_diagnostics = convergence_diagnostics,
    prior_sensitivity = prior_sensitivity,
    posterior_summaries = posterior_summaries,
    posterior_predictive_checks = posterior_predictive_checks,
    model_comparison = model_comparison
  ))
}
