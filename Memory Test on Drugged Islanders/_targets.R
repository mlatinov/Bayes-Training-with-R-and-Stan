
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

  ####  Bayesian Models ####

  ## Multilevel Model with var Intercepts for Memory After
  tar_stan_mcmc(
    name = m_1,
    stan_files = "stan_scripts/bayes_model_1.stan",
    data = list(
      N = nrow(clean_data),
      J = nlevels(clean_data$drug_type),
      Dosage = as.numeric(as.character(clean_data$dosage)),
      Drug_group_idx = as.numeric(as.character(clean_data$drug_type)) + 1,
      Memory = clean_data$memory_score_after,
      Age = clean_data$age
    ),
    chains = 4,
    seed = 123,
    iter_sampling = 1000
    ),

  ## Multilevel Model with var Intercepts for Delta
  tar_stan_mcmc(
    name = m_2,
    stan_files = "stan_scripts/bayes_model_2.stan",
    data = list(
      N = nrow(clean_data),
      J = nlevels(clean_data$drug_type),
      Dosage = as.numeric(as.character(clean_data$dosage)),
      Group_idx = as.numeric(as.character(clean_data$drug_type)) + 1,
      Delta_score = clean_data$delta_score,
      Age = clean_data$age
    ),
    chains = 4,
    seed = 123,
    iter_sampling = 1000
  )
)
