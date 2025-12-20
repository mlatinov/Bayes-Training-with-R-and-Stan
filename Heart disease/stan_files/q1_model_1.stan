
// Variables in the model
data {
  // Number of Observations
  int<lower = 1> N;

  // Exposure
  vector[N] Cholesterol;

  // Adjustment set
  array[N] int<lower = 0,upper = 3> Chest_pain;
  array[N] int<lower = 0,upper = 1> Angina;
  array[N] int<lower = 0,upper = 3> Number_Vessels;
  array[N] int<lower = 0,upper = 2> ST_slope;
  vector[N] ST_depression;

  // Outcome
  array[N] int<lower = 0,upper = 1> Diagnosis;
}

// Model parameters
parameters {
  real beta_0;
  real beta_cholesterol;
  real beta_st_depression;
  vector[4] beta_chest_pain;
  vector[2] beta_angina;
  vector[4] beta_n_vessels;
  vector[3] beta_st_slope;
}

// Model Definition
model {
  // Priors
  beta_0 ~ normal(0,1);
  beta_cholesterol ~ normal(0,1);
  beta_chest_pain ~ normal(0,1);
  beta_angina ~ normal(0,1);
  beta_n_vessels ~ normal(0,1);
  beta_st_depression ~ normal(0,1);
  beta_st_slope ~ normal(0,1);

  // Model Likelihood
  for (n in 1:N) {
    Diagnosis[n] ~ bernoulli_logit(
      beta_0 + beta_chest_pain[Chest_pain[n] + 1 ] +
      beta_cholesterol * Cholesterol[n] +
      beta_angina[Angina[n] + 1] +
      beta_n_vessels[Number_Vessels[n] + 1 ]+
      beta_st_depression * ST_depression[n] +
      beta_st_slope[ST_slope[n] + 1]
    );
  }
}

