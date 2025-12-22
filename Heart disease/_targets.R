#### Global Libraries ####
library(tidyverse)
library(targets)
library(stantargets)

#### Source Function ####
tar_source("functions/")

#### Pipeline ####
list(

  #### Load the dataset ####
  tar_target(
    name = raw_data,
    command = read_csv("data/archive (11)/heart_data.csv")
  ),

  #### Causal model ####
  tar_target(
    name = dag,
    command = create_dag()
  ),

  #### Clean the Data ####
  tar_target(
    name = clean_data,
    command = clean_raw_data(raw_data)
    ),

  #### EDA ####
  tar_target(
    name = eda,
    command = do_eda(clean_data)
  ),

  #### Stan Pipeline ####

  ##### Q1 What is the Direct effect of Cholesterol on Diagnosis #####
  tar_stan_mcmc(
    name = q1_model_1,
    stan_files = "stan_files/q1_model_1.stan",
    data = list(
      N = nrow(clean_data),
      Diagnosis = as.integer(as.character(clean_data$diagnosis)),
      Cholesterol = clean_data$cholesterol,
      ST_slope = as.integer(as.character(clean_data$st_slope)),
      ST_depression = clean_data$st_depression,
      Angina = as.integer(as.character(clean_data$exercise_angina)),
      Number_Vessels = as.integer(as.character(clean_data$num_vessels)),
      Chest_pain = as.integer(as.character(clean_data$chest_pain))
      ),
    chains = 4,
    iter_sampling = 1000,
    seed = 123
    ),

  ##### Q2 What is the Total effect of Cholesterol on Diagnosis #####
  tar_stan_mcmc(
    name = q2_model_1,
    stan_files = "stan_files/q2_model_1.stan",
    data = list(
      N = nrow(clean_data),
      Diagnosis = as.integer(as.character(clean_data$diagnosis)),
      Cholesterol = clean_data$cholesterol,
      Age = clean_data$age,
      Gender = as.integer(as.character(clean_data$gender))
      ),
    chains = 4,
    iter_sampling = 1000,
    seed = 123
    ),

  ##### Q3 What is the Direct Effect of Age on Diagnosis #####
  tar_stan_mcmc(
    name = q3_model_1,
    stan_files = "stan_files/q3_model_1.stan",
    data = list(
      N = nrow(clean_data),
      Diagnosis = as.integer(as.character(clean_data$diagnosis)),
      Age = clean_data$age,
      Exercise_angina = as.integer(as.character(clean_data$exercise_angina)),
      Chest_pain = as.integer(as.character(clean_data$chest_pain)),
      N_vessels = as.integer(as.character(clean_data$num_vessels)),
      ST_slope = as.integer(as.character(clean_data$st_slope)),
      ST_depression = clean_data$st_depression
    ),
    chains = 4,
    iter_sampling = 1000,
    seed = 123
  ),

)
