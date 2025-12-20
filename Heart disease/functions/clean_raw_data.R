
#### Function to Clean the Raw data ####
clean_raw_data <- function(raw_data){

  raw_data %>%

    ## Change data types ##
    mutate(

      # Gender is a factor with level 0 female and 1 male
      gender = as.factor(gender),

      # ST Slope is a factor 0 = downsloping, 1 = flat, 2 = upsloping.
      st_slope = as.factor(st_slope),

      # Fasting blood sugar is a factor level 0 normal levels of glucose 1 is annomaly
      fasting_bs = as.factor(fasting_bs),

      # Angina triggered by exercise is a factor 0 No indication 1 present
      exercise_angina = as.factor(exercise_angina),

      # Chest Pain is a factor with 3 Levels 0 assumed absense of pain
      chest_pain = as.factor(chest_pain),

      # Rest ECG Detects electrical abnormalities levels 0 and 1
      rest_ecg = as.factor(ifelse(test = rest_ecg == 0,yes = 0,no = 1)),

      # Diagnosis is a Factor with levels 0 and 1
      diagnosis = as.factor(diagnosis)
    )
}
