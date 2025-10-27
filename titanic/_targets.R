
#### 1 Libraries ####
library(tidyverse)
library(dagitty)
library(causaldata)
library(targets)
library(cmdstanr)
library(brms)
library(bayesplot)

# Load functions
source("functions/simulate_titanic_function.R")
source("functions/check_gen_model.R")
source("functions/b_model_age.R")
source("functions/conditional_prob.R")

#### 2 Pipeline ####
list(

  #### 3 Get the data ####
  tar_target(
    name = titanic_dataset,
    command = causaldata::titanic
    ),

  ### 4 Make a DAG ####
  tar_target(
    name = titanic_dag,
    command =
    dagitty('dag {
    Sex -> Class -> Survive
    Age -> Class
    Age -> Survive
    Sex -> Survive
}
')
    ),
  #### 5 Generative  Model ####
  tar_target(
    name = titanic_gen_model,
    command = simulate_titanic(10000)
    ),

  ## Check the Generative Model ##
  tar_target(
    name = check_gen_plots,
    command = check_gen_model(titanic_gen_model)
  ),
  #### BRMS Model for estimating the direct effect of Age to Survival ####

  # With the actual data
  tar_target(
    name = model_age,
    command = b_model_age(titanic_dataset))
)
