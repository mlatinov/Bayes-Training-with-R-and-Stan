
#### Libraries ####
library(targets)
library(tidyverse)

#### Source Functions ####
tar_source(files = "functions/")

#### Pipeline ####
list(

  ## Load the data ##
  tar_target(
    name = howell_data,
    command = read_delim("data/Howell1.csv",
      delim = ";",
      escape_double = FALSE,
      trim_ws = TRUE)
    ),

  ## Causal model DAG ##
  tar_target(
    name = howell_dag,
    command = howell_make_dag(howell_data)
    ),

  ## Generative model ##
  tar_target(
    name = howell_gen,
    command = howell_make_gen(544)
  ),

  ## Clean Explore and Compare the data to the Generative model  ##
  tar_target(
    name = howell_eda,
    command = eda_howell(howell_data,howell_gen)
  ),

  # BRMS Model 1 Model Only Height ~ Weight
  tar_target(
    name = b_model_weight,
    command = b_model_w(howell_eda$datasets$howell_clean)
    ),

  # RRMS Model 2 Height ~ Weight + Age
  tar_target(
    name = b_model_weight_age,
    command = b_model_wa(howell_eda$datasets$howell_clean)
    )



)
