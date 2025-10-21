
#### Function to Check the Generative Model ####
check_gen_model <- function(gen_model){

  # Check the Survival by Age and expect the most of the survived to be Children
  survival_rates <- gen_model %>%
    group_by(age) %>%
    summarise(
      n = n(),
      survived = sum(survived),
      survival_rate = survived / n * 100
    )
  g1<-
    ggplot(data =survival_rates,aes(x = factor(age),y= survival_rate,fill = factor(age)))+
    scale_fill_viridis_d()+
    geom_col()+
    theme_minimal()+
    labs(
      title = "Survival Rates Adults and Children",
      fill = "Age",
      y = "Survival Rate",
      x = "Age Category"
      )

  # Check the Distribution of Class of Age and Sex
  g2 <-
    ggplot(data = gen_model,aes(x = factor(class),fill = factor(age)))+
    geom_bar()+
    facet_wrap(~sex)+
    scale_fill_viridis_d()+
    theme_minimal()+
    labs(
      title = "Class Distribution by Age and Sex",
      x = "Class",
      y = "Count",
      fill = "Age"
    )
  # Check the Distribution of Class in survival
  g3 <-
    ggplot(data = gen_model,aes(x = factor(class),fill = factor(survived)))+
    geom_bar()+
    scale_fill_viridis_d()+
    theme_minimal()+
    labs(
      title = "Class Distribution by Survival",
      x = "Class",
      y = "Count",
      fill = "Survived"
    )

  # Return all plots
  return(
    list(
    survival_by_age = g1,
    distribution_class_age_sex = g2,
    distribution_class_survival = g3
  ))
}
