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

  # Extract posterior draws
  post_draws <- as_draws_df(model)

  # Convergence checks
  trace_plot <- mcmc_trace(post_draws, pars = c("b_age1","b_Intercept"))

  # Sample Size check
  effect_sample <- mcmc_neff(neff_ratio(model))

  # Posterior Summaries
  dens <- mcmc_dens_overlay(post_draws, pars = c("b_age1", "b_Intercept"))
  interval <- mcmc_intervals(post_draws, pars = c("b_age1", "b_Intercept"))

  # Posterior Predictive Checks
  pp <- pp_check(model, type = "bars")

  # Influence and Fit
  loo <- loo_result <- plot(loo(model))

  # Effect of age on the log odds scale
  age_effect_log <- post_draws$b_age1

  #### Calculate Conditional probability ####
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

  prob_data_1st_class_male <- data.frame(
    age_group = rep(c("Adult", "Child"), each = length(prob_adult_1st_class_male)),
    probability = c(prob_adult_1st_class_male, prob_child_1st_class_male)
  )
  prob_data_1st_class_female <- data.frame(
    age_group = rep(c("Adult", "Child"), each = length(prob_adult_1st_class_female)),
    probability = c(prob_adult_1st_class_female, prob_child_1st_class_female)
  )

  # Store plots for Age effect
  effect_plots <- list()

  # Age Posterior Distribution by groups Marginal Effect of age
  effect_plots$age_posterior_prob_1st_class_male <-
    ggplot(data = prob_data_1st_class_male,aes(x = probability,fill = age_group))+
    geom_density(alpha = 0.5)+
    theme_minimal()+
    labs(
      title = "Age Posterior Distributions of meles by Age groups",
      x = "Probability of Survival",
      y = "Density",
      fill = "Age Group"
    )

  effect_plots$age_posterior_prob_1st_class_female <-
    ggplot(data = prob_data_1st_class_female,aes(x = probability,fill = age_group))+
    geom_density(alpha = 0.5)+
    theme_minimal()+
    labs(
      title = "Age Posterior Distribution of females by Age groups",
      x = "Probability of Survival",
      y = "Density",
      fill = "Age Group"
    )

  #### Marginal probability ####

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
  effect_plots$marginal_prob <-
  ggplot(data = marginal_summary, aes(x = age, y = mean_probability)) +
    geom_pointrange(aes(ymin = lower_95, ymax = upper_95),size = 1, color = "darkred") +
    labs(
      title = "Marginal Age Effects",
      subtitle = "Average survival probability across all sex and class combinations",
      x = "Age Group",
      y = "Survival Probability") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()

  # Age Posterior Distribution in Log odds scale
  effect_plots$age_posterior <-
    ggplot(data = as.data.frame(age_effect_log),aes(x =age_effect_log))+
    geom_density(fill = "lightblue")+
    geom_vline(xintercept = mean(age_effect_log),colour = "black",linetype = "dashed")+
    theme_minimal()+
    labs(
      title = "Age Posterior Distribution in log odds scale",
      x = "Desity",
      y = "Log Odds"
    )

  # Return list
  return(list(
    trace_plot = trace_plot,
    effect_sample = effect_sample,
    density_overlay = dens,
    posterior_interval = interval,
    posterior_predictive = pp,
    loo = loo,
    effect_plots = effect_plots
  ))
}

