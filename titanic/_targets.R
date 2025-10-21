
#### 1 Libraries ####
library(tidyverse)
library(dagitty)
library(causaldata)
library(targets)
library(cmdstanr)

# Load functions
source("functions/simulate_titanic_function.R")
source("functions/check_gen_model.R")

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
    command = simulate_titanic(n = 10000)
    ),

  ## Check the Generative Model ##
  tar_target(
    name = check_gen_plots,
    command = check_gen_model(titanic_gen_model)
  )
)
