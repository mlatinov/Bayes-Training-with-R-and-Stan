
#### Function to Perform EDA ####
eda_performace <- function(data){

  ## Global Sets ##
  set_theme(theme_minimal())

  #### Univariate analysis ####

  ### Variable Distributions ###

  # Test Final Scores
  test_result_d <-
    ggplot(data = data, aes(x = test_result))+
    geom_histogram(colour = "black",fill = "lightblue")+
    labs(
      title = "Test Result Distribution",
      x = "Test Result Scores",
      y = "Count"
    )

  # Previous Scores
  previous_score_d <-
    ggplot(data = data, aes(x = previous_score))+
    geom_histogram(colour = "black",fill = "lightblue")+
    labs(
      title = "Previous Score Distribution",
      x = "Previous Score ",
      y = "Count"
    )

  #### Bivariate Analysis ####

  ## Test Results ~ Previous Score
  test_final_h_studied <-
    ggplot(data = data, aes(x = previous_score,y = test_result))+
    geom_point(alpha = 0.2)+
    geom_smooth(method = "lm",se = FALSE,colour = "red")+
    labs(
      title = "Test Scores Final ~ Previus Scores ",
      x = "Previous Scores",
      y = "Final Scores"
    )

  # Return
  return(list(

    # Distributions
    distributions = list(
      final_test_results = test_result_d,
      previous_score = previous_score_d
    ),

    # Relantionships
    relantionships = list(
      test_scores_previous_test = test_final_h_studied
    )
  ))
}
