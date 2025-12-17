
#### Function to Clean the data ####
clean_data <- function(data){

  data %>%

    # Rename the variables
    rename(
      h_studied = Hours.Studied,
      previous_score = Previous.Scores,
      extra_activities = Extracurricular.Activities,
      sleep_h = Sleep.Hours,
      question_practice = Sample.Question.Papers.Practiced,
      test_result = Performance.Index
    ) %>%

    # Convert extra_activities to factor with 0 and 1
    mutate(
      extra_activities = as.factor(
        if_else(extra_activities == "Yes", true = 1, false = 0)
        )
    )
}
