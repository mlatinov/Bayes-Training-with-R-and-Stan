
#### Libraries ####
library(targets)
library(tidyverse)
library(brms)
library(bayesplot)
library(posterior)
library(dagitty)
library(ggdag)
library(splines)
library(patchwork)

#### Source Functions ####
source("functions/howell_make_dag.R")
source("functions/howell_make_gen.R")
source("functions/b_model_weight_function.R")

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

  ## Clean and explore the data ##
  tar_target(
    name = howell_clean,
    command = eda_howell(howell_data)
  ),

  ## Causal model DAG ##
  tar_target(
    name = howell_dag,
    command = howell_make_dag(howell_data)
    ),

  ## Generative model ##
  tar_target(
    name = howell_gen,
    command = howell_make_gen(10000)
  ),

  # BRMS Model 1 Model Only Height ~ Weight
  tar_target(
    name = b_model_weight,
    command = b_model_w(howell_data)
    )
)
