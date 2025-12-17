
#### Libraries ####
library(targets)
library(cmdstanr)
library(tidyverse)


#### Sourse Functions ####
tar_source("scripts/")

#### Pipeline ####
list(

  ## Load the dataset ##
  tar_target(
    name = student_performance,
    command = read.csv("data/Student_Performance.csv")
  ),

  ## Clean the data ###
  tar_target(
    name = clean_performance,
    command = clean_data(student_performance)
  ),

  ## EDA ##
  tar_target(
    name = eda_plots,
    command = eda_performace(clean_performance)
  ),

  ## Sample for Faster Models ##
  tar_target(
    name = sample_performance,
    command = sample_n(clean_performance,size = 500,replace = FALSE)
  ),

  #### Stan Models ####

  ## Final Scores ~ Previous Scores ##
  tar_target(
    name = model_1,
    command = stan_model_1(sample_performance,file = "stan_code/stan_model_1.stan")
  ),

  ## Final Scores ~ Previous Scores 2 ##
  tar_target(
    name = model_1_1,
    command = stan_model_1(sample_performance, file = "stan_code/stan_model_1_1.stan")
  )
)
