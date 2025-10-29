##### BRMS Model for estimating the direct effect of age on the survival ####

b_model_age <- function(data){

  # Convert every variable into factor
  data <- data %>%
    mutate(
      across(everything(),.fns = as.factor)
    )

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


  #### MODEL and  CONVERGENCE ####
  # Extract posterior draws
  post_draws <- as_draws_df(model)

  modeling <- list(
    model_object = model,
    summary = summary(model),
    post_draws = post_draws,
    formula = formula(model)
  )

  #### CONVERGENCE DIAGNOSTICS ####

  # Convergence checks
  trace_plot <- mcmc_plot(model,type = "trace")+
    ggtitle(label = "Convergence Trace Plot ")+
    theme_minimal()

  # Sample Size check
  effect_sample <- mcmc_neff(neff_ratio(model))+
    ggtitle(label = "Effect Sample Size ")+
    theme_minimal()

  # Posterior Summaries
  posterior_samples_per_param <- mcmc_plot(model,type = "hist",bins = 30)+
    ggtitle(label = "Posterior Summaries")+
    theme_minimal()

  diagnostics <- list(
    trace_plot = trace_plot,
    effect_sample = effect_sample,
    posterior_samples = posterior_samples_per_param,
    rhat = rhat(model),
    neff = neff_ratio(model)
  )

  #### POSTERIOR PREDICTIVE CHECKS ####

  # Posterior Predictive Checks
  pp_pred <- pp_check(model, type = "bars")

  # Posterior Error Checks
  pp_error <- pp_check(model, type = "error_binned")

  # Posterior Stats Checks
  pp_mean <- pp_check(model, type = "stat", stat = "mean")
  pp_sd <- pp_check(model, type = "stat", stat = "sd")
  pp_group_sex <- pp_check(model, type = "stat_grouped", group = "sex", stat = "mean")
  pp_group_class <- pp_check(model, type = "stat_grouped", group = "class", stat = "mean")

  predictive_checks <- list(
    pp_pred = pp_pred,
    pp_error = pp_error,
    pp_mean = pp_mean,
    pp_sd = pp_sd,
    pp_group_sex = pp_group_sex,
    pp_group_class = pp_group_class
  )

  #### CONDITIONAL EFFECTS ####

  # Conditional Effect of posterior fitted means
  effect_post_means <- conditional_effects(model,method = "posterior_epred")

  # Conditional Effect Full predictive draws
  effect_post_pred <- conditional_effects(model,method = "posterior_predict")

  # Calculate Conditional probability #
  prob_adult_1st_class_male <- conditional_prob(
    dataset = data,
    post_draws = post_draws,
    age = "1",
    sex = "1",
    class = "1"
  )
  prob_child_1st_class_male <- conditional_prob(
    dataset = data,
    post_draws = post_draws,
    age = "0",
    sex = "1",
    class ="1"
  )
  prob_child_1st_class_female <- conditional_prob(
    dataset = data,
    post_draws = post_draws,
    age = "0",
    sex = "0",
    class = "1"
  )
  prob_adult_1st_class_female <- conditional_prob(
    dataset = data,
    post_draws = post_draws,
    age = "1",
    sex = "0",
    class = "1"
  )

  # Wrangle the data
  prob_data_1st_class_male <- data.frame(
    age_group = rep(c("Adult", "Child"), each = length(prob_adult_1st_class_male)),
    probability = c(prob_adult_1st_class_male, prob_child_1st_class_male)
  )
  prob_data_1st_class_female <- data.frame(
    age_group = rep(c("Adult", "Child"), each = length(prob_adult_1st_class_female)),
    probability = c(prob_adult_1st_class_female, prob_child_1st_class_female)
  )

  # Age Posterior Distribution by groups of age
  age_posterior_prob_1st_class_male <-
    ggplot(data = prob_data_1st_class_male,aes(x = probability,fill = age_group))+
    geom_density(alpha = 0.5)+
    theme_minimal()+
    labs(
      title = "Age Posterior Distributions of males by Age groups in the 1st class",
      x = "Probability of Survival",
      y = "Density",
      fill = "Age Group"
    )

  age_posterior_prob_1st_class_female <-
    ggplot(data = prob_data_1st_class_female,aes(x = probability,fill = age_group))+
    geom_density(alpha = 0.5)+
    theme_minimal()+
    labs(
      title = "Age Posterior Distribution of females by Age groups in the 1st class",
      x = "Probability of Survival",
      y = "Density",
      fill = "Age Group"
    )

  # Get everything together to return as a list
  conditional_effects <- list(
    fitted_means = effect_post_means,
    predictive_draws = effect_post_pred,
    specific_probabilities = list(
      adult_male_1st = prob_adult_1st_class_male,
      child_male_1st = prob_child_1st_class_male,
      child_female_1st = prob_child_1st_class_female,
      adult_female_1st = prob_adult_1st_class_female
      ),
    plots = list(
      cond_ef_age_by_male = age_posterior_prob_1st_class_male,
      cond_ef_age_by_female = age_posterior_prob_1st_class_female
      )
    )

  #### MARGINAL EFFECTS ####

  # Get all possible combinations
  grid <- expand.grid(
    age = levels(data$age),
    sex = levels(data$sex),
    class = levels(data$class)
  )

  # Calculate probability for each combination
  prob <- list()
  for (i in 1:nrow(grid)) {
    combination <- grid[i,]
    prob[[i]] <- conditional_prob(
      dataset = data,
      post_draws = post_draws,
      age = as.character(combination$age),
      sex = as.character(combination$sex),
      class = as.character(combination$class)
    )
  }

  # Group by age and average
  marginal_prob <- data.frame(
    age = grid$age,
    probability = unlist(lapply(prob,mean))
  )

  # Calculate overall average by age
  marginal_summary <- marginal_prob %>%
    group_by(age) %>%
    summarise(
      mean_probability = mean(probability),
      lower_95 = quantile(probability, 0.025),
      upper_95 = quantile(probability, 0.975)
    )

  # Display Marginal Probability
  marginal_prob <-
    ggplot(data = marginal_summary, aes(x = age, y = mean_probability)) +
    geom_pointrange(aes(ymin = lower_95, ymax = upper_95),size = 1, color = "darkred") +
    labs(
      title = "Marginal Age Effects",
      subtitle = "Average survival probability across all sex and class combinations",
      x = "Age Group",
      y = "Survival Probability") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()


  # Return list with marginal summaries and stats
  marginal_effects <- list(
    marginal_summary = marginal_summary,
    plots = list(
      marginal_age_effect = marginal_prob
    )
  )

  #### MODEL COMPARISON & FIT ####
  loo <- loo(model)

  model_comparison <- list(
    loo_result = loo,
    pareto_k_plot = plot(loo),
    loo_summary = data.frame(
      Metric = c("ELPD", "p_loo", "LOOIC"),
      Value = loo$estimates[, "Estimate"],
      SE = loo$estimates[, "SE"]
      ),
    bayes_r = bayes_R2(model)
  )

  # Return list
  return(list(
    model = modeling,
    convergence_diagnostics = diagnostics,
    posterior_predictive_checks = predictive_checks,
    conditional_effects = conditional_effects,
    marginal_effects = marginal_effects,
    model_comparison = model_comparison
    ))
}

