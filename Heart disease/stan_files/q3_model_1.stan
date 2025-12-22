
// Model Variables
data {
  int N; // Number of Observations

  // Exposure
  vector[N] Age;

  // Adjustment Set
  array[N] int<lower = 0,upper = 3> Chest_pain;
  array[N] int<lower = 0,upper = 1> Exercise_angina;
  array[N] int<lower = 0,upper = 3> N_vessels;
  array[N] int<lower = 0,upper = 2> ST_slope;
  vector[N] ST_depression;

  // Outcome
  array[N] int<lower = 0,upper = 1> Diagnosis ;
}

// Variable Transformations
transformed data {
  // Standartize Continues Variables
  vector[N] Z_st_depression;
  Z_st_depression = (ST_depression - mean(ST_depression)) / sd(ST_depression);
}

// Model Parameters
parameters{
  real beta_0; // Global Intercept
  real beta_st_depression;
  real beta_exercise_angina;
  real beta_age;

  // Raw Categorical Non-Zero Parameters
  vector[3] beta_n_vessels_non_zero; // 3 levels without 0
  vector[3] beta_chest_pain_non_zero; // 3 levels without 0
  vector[2] beta_st_slope_non_zero; // 2 levels without 0
}
// Parameter Transformation
transformed parameters{
  // Add 0 as a Ref Level to all Categorical parameters
  vector[4] beta_n_vessels;
  beta_n_vessels[1] = 0;
  beta_n_vessels[2] = beta_n_vessels_non_zero[1];
  beta_n_vessels[3] = beta_n_vessels_non_zero[2];
  beta_n_vessels[4] = beta_n_vessels_non_zero[3];

  vector[4] beta_chest_pain;
  beta_chest_pain[1] = 0;
  beta_chest_pain[2] = beta_chest_pain_non_zero[1];
  beta_chest_pain[3] = beta_chest_pain_non_zero[2];
  beta_chest_pain[4] = beta_chest_pain_non_zero[3];

  vector[3] beta_st_slope;
  beta_st_slope[1] = 0;
  beta_st_slope[2] = beta_st_slope_non_zero[1];
  beta_st_slope[3] = beta_st_slope_non_zero[2];
}

// Model Specification
model{

  // Priors
  beta_0 ~ normal(0,1);
  beta_age ~ normal(0,1);
  beta_st_depression ~ normal(0,1);
  beta_exercise_angina ~ normal(0,1);
  beta_n_vessels_non_zero ~ normal(0,1);
  beta_chest_pain_non_zero ~ normal(0,1);
  beta_st_slope_non_zero ~ normal(0,1);

  // Model Likelyhood
  for(n in 1:N){
    Diagnosis[n] ~ bernoulli_logit(
      beta_0 +
      beta_age * Age[n] +
      beta_st_depression * Z_st_depression[n] +
      beta_exercise_angina * Exercise_angina[n]  +
      beta_n_vessels[N_vessels[n] + 1] +
      beta_chest_pain[Chest_pain[n] + 1] +
      beta_st_slope[ST_slope[n] + 1]
    );
  }
}

// Additional Model Output
generated quantities{
  // Predictive Distribution
  array[N] int y_rep;

  // Loop to generate data from the model
  for(n in 1:N){
    real p;
    p = inv_logit(
      beta_0 +
      beta_age * Age[n] +
      beta_st_depression * Z_st_depression[n] +
      beta_exercise_angina * Exercise_angina[n]  +
      beta_n_vessels[N_vessels[n] + 1] +
      beta_chest_pain[Chest_pain[n] + 1] +
      beta_st_slope[ST_slope[n] + 1]
    );
  // Sample from p
  y_rep[n] = bernoulli_rng(p);
  }

  // Age effect on Diagnosis Age 20 - Age 50
  real age_20 = 20;
  real age_50 = 50;

  // Refrence Levels
  int chest_pain_ref = 0;
  int exercise_angina_ref = 0;
  int n_vessels_ref = 0;
  int st_slope_ref = 0;
  real st_depression_ref = 0;

  // Calculate the p for Age20 and Age50
  real p_20;
  real p_50;

  p_20 = inv_logit(
    beta_0 +
      beta_age * age_20 +
      beta_st_depression * st_depression_ref +
      beta_exercise_angina * exercise_angina_ref  +
      beta_n_vessels[n_vessels_ref + 1] +
      beta_chest_pain[chest_pain_ref + 1] +
      beta_st_slope[st_slope_ref + 1]
  );

  p_50 = inv_logit(
    beta_0 +
      beta_age * age_50 +
      beta_st_depression * st_depression_ref +
      beta_exercise_angina * exercise_angina_ref  +
      beta_n_vessels[n_vessels_ref + 1] +
      beta_chest_pain[chest_pain_ref + 1] +
      beta_st_slope[st_slope_ref + 1]
  );

  // Compute the diffrence
  real delta_p_20_50;

  delta_p_20_50 = p_50 - p_20;
}
