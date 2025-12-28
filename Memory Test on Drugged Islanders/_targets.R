
#### Libraries ####
library(tidyverse)
library(targets)
library(stantargets)

#### Source functions ####
tar_source("functions/")

#### Pipeline ####
list(

  #### Load the Dataset
  tar_target(
    name = data,
    command = read.csv("data/Islander_data.csv")
  ),

  #### Clean the data ####
  tar_target(
    name = clean_data,
    command = clean_islanders(data)
  ),

  #### EDA ####
  tar_target(
    name = eda_data,
    command = eda_islanders(clean_data)
  ),

  ####  Bayesian One-Way Anova ####

  ## Memory After And Drug Type Log-normal Model
  tar_stan_mcmc(
    name = aov_after_drug_type,
    stan_files = "stan_scripts/bayes_aov_1.stan",
    data = list(
      N = nrow(clean_data),
      J = nlevels(clean_data$drug_type),
      group_idx = as.numeric(as.character(clean_data$drug_type)) + 1,
      Memory_score = clean_data$memory_score_after
    ),
    chains = 4,
    iter_sampling = 1000,
    seed = 123
    ),

  ## Memory After And Drug Type Normal Model
  tar_stan_mcmc(
    name = aov_after_drug_type_2,
    stan_files = "stan_scripts/bayes_aov_2.stan",
    data = list(
      N = nrow(clean_data),
      J = nlevels(clean_data$drug_type),
      Group_idx = as.numeric(as.character(clean_data$drug_type)) + 1,
      Memory_score = clean_data$memory_score_after
    ),
    chains = 4,
    iter_sampling = 1000,
    seed = 123
  ),

  ## Delta Score and Drug Type
  tar_stan_mcmc(
    name = aov_delta_drug_type,
    stan_files = "",
    data = list(),
    chains = 4,
    iter_sampling = 1000,
    seed = 123
  )

)
