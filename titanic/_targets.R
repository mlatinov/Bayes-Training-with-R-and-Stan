
#### 1 Libraries ####
library(tidyverse)
library(dagitty)
library(causaldata)
library(targets)
library(cmdstanr)


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
  #### 5 Scientific Model ####
  tar_target(
    name = titanic_sc_model,
    command = simulate_titanic()
    )

)
