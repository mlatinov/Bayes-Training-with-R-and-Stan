
#### Function for EDA ####
do_eda <- function(clean_data){


  #### Libraries ####
  library(patchwork)

  # Global Sets
  theme_set(theme_minimal())

  #### Univariate Analysis ####

  # ST Depression distribution
  st_depression_d <-
    ggplot(data = clean_data,aes(x = st_depression))+
    geom_histogram(colour = "black",fill = "pink")+
    labs(
      title = "ST Depression Distribution"
      )

  # ST Depression Boxplot
  st_depression_boxplot <-
    ggplot(data = clean_data,aes(x = st_depression))+
    geom_boxplot(fill = "pink",outlier.color = "red")+
    labs(
      title = "ST Depression Boxplot"
    )

  # Combine
  univ_st_depression <- st_depression_d + st_depression_boxplot

  # Cholesterol Distribution
  cholesterol_d <-
    ggplot(data = clean_data,aes(x = cholesterol))+
    geom_histogram(colour = "black",fill = "pink")+
    labs(
      title = "Cholesterol Distribution"
    )

  # Cholesterol Boxplot
  cholesterol_boxplot <-
    ggplot(data = clean_data,aes(x = cholesterol))+
    geom_boxplot(fill = "pink",outlier.color = "red")+
    labs(
      title = "Cholesterol Boxplot"
    )

  # Combine
  univ_cholesterol <- cholesterol_d + cholesterol_boxplot

  #### Bivariate Analysis ####

  ## Diagnosis ~ Cholesterol ##
  diagnosis_cholesterol_table <-
    clean_data %>%
    select(diagnosis,cholesterol) %>%
    group_by(diagnosis) %>%
    summarise(
      mean_cholesterol = mean(cholesterol),
      median_cholesterol = median(cholesterol),
      sd_cholesterol = sd(cholesterol),
      var_cholesterol = var(cholesterol)
    )

  ## Boxplot Diagnosis ~ Cholesterol ##
  diagnosis_cholesterol_boxplot <-
    ggplot(data = clean_data,aes(x = cholesterol,fill = diagnosis))+
    scale_fill_discrete(palette = c("pink","red"))+
    geom_boxplot()+
    labs(
      title = "Cholesterol And Diagnosis",
      x = "Cholesterol",
      fill = "Diagnosis"
    )

  ## Cholesterol ~ Age
  cholesterol_age <-
    ggplot(data = clean_data,aes(x = age,y = cholesterol))+
    geom_point(colour = "pink")+
    geom_smooth(method = "lm",se = FALSE)+
    labs(
      title = "Cholesterol ~ Age",
      x = "Age",
      y = "Cholesterol"
    )

  ## Cholesterol ~ Gender
  cholesterol_gender <-
    ggplot(data = clean_data,aes(x = cholesterol,fill = gender))+
    geom_boxplot()+
    scale_fill_discrete(palette = c("pink","red"))+
    labs(
      title = "Cholesterol ~ Gender",
      fill = "Gender",
      x = "Cholesterol"
    )

  #### Multivariate Analysis ####

  ## Diagnosis ~ Cholesterol + Age + Gender
  diagnosis_cholesterol_age <-
    ggplot(data = clean_data,aes(y = cholesterol,x = age,color = diagnosis))+
    geom_point()+
    facet_wrap(~gender)+
    labs(
      title = "Diagnosis ~ Age + Gender",
      x = "Age",
      y = "Cholesterol",
      fill = "Diagnosis"
    )


  # Return
  return(list(
    univariate_analysis = list(
      st_depression = univ_st_depression,
      cholesterol = univ_cholesterol
    ),
    bivariate_analysis = list(
      diagnosis_x = list(
        stat_table = diagnosis_cholesterol_table,
        boxplot = diagnosis_cholesterol_boxplot
      ),
      cholesterol_x = list(
        cholesterol_age,
        cholesterol_gender
      )
    ),
    multivariate_analysis = list(
      diagnosis_cholesterol_age
    )
  ))
}
