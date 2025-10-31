
#### BRMS Model Modeling Height ~ Weight ####
b_model_w <- function(data){

  #### Specify Formula ####
  formula <- brmsformula(height ~ 1 + weight,family = gaussian)

  #### Specify Priors ####
  priors <- c(
    prior(normal(1,0.5),class = "b",coef = "weight"),
    prior(normal(120,25),class = "Intercept"),
    prior(student_t(10,0,6),class = "sigma")
  )

  #### Prior Predictive sim ####
  prior_sim <- brm(
    formula = formula,
    data = howell_data,
    family = gaussian,
    prior = priors,
    sample_prior = "only",
    chains = 4,
    iter = 2000,
    cores = 4,
    seed = 123
    )

  # Check Plausibility of predictions (Prior Predictive Distribution)
  pp_sim <- pp_check(prior_sim,type = "hist")+
    ggtitle("Prior Predictive Distribution")+
    theme_minimal()

  # Check Effects of predictors
  conditional_effect_sim <- conditional_effects(prior_sim)

  # Prior parameters Distributions
  prior_draws <- as_draws_df(prior_sim)
  p1 <- mcmc_areas(prior_draws, pars = "b_Intercept") + ggtitle("Intercept Prior")+ theme_minimal()
  p2 <- mcmc_areas(prior_draws, pars = "b_weight") + ggtitle("Weight Coefficient Prior") + theme_minimal()
  p3 <- mcmc_areas(prior_draws, pars = "sigma") + ggtitle("Sigma Prior") + theme_minimal()

  # Combine the plots
  prior_param <- (p1 + p2) / p3

  # Collect everything in a list
  prior_simulation <- list(
    prior_model = prior_sim,
    plots = list(
      plausibility_of_predictions = pp_sim,
      parameter_distribution = prior_param,
      conditional_effect_weight = conditional_effect_sim
    )
  )

  #### BRMS Model ####
  model_w <- brm(
    formula = formula,
    data = howell_data,
    family = gaussian,
    prior = priors,
    chains = 4,
    iter = 2000,
    cores = 4,
    seed = 123
    )

  #### MODEL ####

  # Extract the posterior draws
  posterior_draws <- as_draws_df(model_w)

  # Collect in a list
  model <- list(
    model = model_w,
    summary  = summary(model_w),
    posterior_draws = posterior_draws,
    formula = formula
  )

  #### CONVERGENCE DIAGNOSTICS ####

  # Trace Plots
  trace_plot <- mcmc_plot(model_w,type = "trace")+
    ggtitle("Trace plot")+
    theme_minimal()

  # Autocorrection of the chains
  auto_chain <- mcmc_acf(model_w)+
    ggtitle("Autocorrection of the Chains")+
    theme_minimal()

  # Effective Sample Ratio
  effective_sample <- mcmc_neff(neff_ratio(model_w))+
    ggtitle("Effective Sample size Ratio")+
    theme_minimal()

  # Collect in a list
  convergence_diagnostics <- list(
    trace_plots = trace_plot,
    autocorrection = auto_chain,
    effective_sample = effective_sample
  )

  #### POSTERIOR Summaries ####

  # Posterior Distributions
  posterior_dist <- mcmc_plot(model_w,type = "hist")+
    ggtitle("Posterior Distributions")+
    theme_minimal()

  # Credible Intervals
  intervals <- mcmc_plot(model_w,type = "intervals",prob = 0.88)+
    ggtitle("88% Credible intervals")+
    theme_minimal()

  # Numerical summary
  post_summary <- posterior_summary(model_w)

  # Collect posterior summary in a list
  posterior_summaries <- list(
    posterior_distributions = posterior_dist,
    credible_interval = intervals,
    posterior_summary = post_summary
  )

  #### Posterior Predictive Checks ####

  # Posterior Predictive Distribution
  pp_dens <- pp_check(model_w,type = "dens_overlay")+
    ggtitle("Posterior Predictive Distribution")+
    theme_minimal()

  # Posterior Predictive Error
  pp_error <- pp_check(model_w,type = "error_binned" )+
    ggtitle("Posterior Predictive Error")+
    theme_minimal()

  # Posterior Stats Checks
  pp_mean <- pp_check(model_w, type = "stat", stat = "mean")
  pp_sd <- pp_check(model_w, type = "stat", stat = "sd")

  # Collect pp checks in a list
  posterior_predictive_checks <- list(
    post_pred_density = pp_dens,
    posterior_error = pp_error,
    post_stats = list(
      mean = pp_mean,
      sd = pp_sd
    )
  )

  #### MODEL COMPARISON & FIT ####
  loo <- loo(model_w)

  # Collect in a list
  model_comparison <- list(
    loo_result = loo,
    pareto_k_plot = plot(loo),
    loo_summary = data.frame(
      Metric = c("ELPD", "p_loo", "LOOIC"),
      Value = loo$estimates[, "Estimate"],
      SE = loo$estimates[, "SE"]
    ),
    bayes_r = bayes_R2(model_w)
  )


  #### Hypothesis Testing ####

  # Hypothesis 1 : The effect of weight on height is positive
  hyp_1 <- hypothesis(model_w, "weight > 0")

  # Return Everything collected
  return(list(
    model = model,
    convergence_diagnostics = convergence_diagnostics,
    posterior_summaries = posterior_summaryes,
    posterior_predictive_checks = posterior_predictive_checks,
    model_comparison = model_comparison,
    hypothesis_testing = list(
      weight_effect_above_0 = hyp_1)
  ))
}

