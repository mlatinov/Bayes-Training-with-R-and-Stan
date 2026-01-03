
// Multilevel Model with Varing Intercepts by Drug Groups

// Model Variables
data{
  int<lower = 1> N;               // N of observations
  int<lower = 1> J;               // Number of Groups
  array[N] int<lower = 1,upper = J> Group_idx;  // Drug Group index per observation
  array[N] int<lower = 0,upper = 2> Dosage; // Drug Dosage Groups
  vector[N] Delta_score;                // Outcome
  vector[N] Age;                        // Age of the subjects
}

// Variable Transformations
transformed data {
  // Continues Variable Z Standartization
  vector[N] Z_age;
  Z_age = (Age - mean(Age)) / sd(Age);
}

// Model Parameters
parameters{

  // Alpha Hyper Parameters
  real mu_alpha;
  real<lower =0> sd_alpha;
  vector[J] z_alpha;

  // Variable Effects
  real beta_age;
  vector[2] beta_dosage_raw;

  // Population SDs
  real<lower =0> sigma;
}

// Transformed Parameters
transformed parameters{

  // Non-centered alpha
  vector[J] alpha;
  alpha = mu_alpha + sd_alpha * z_alpha;

  // Ref Encoding for Categorical Parameters
  vector[3] beta_dosage;
  beta_dosage[1] = 0;
  beta_dosage[2] = beta_dosage_raw[1];
  beta_dosage[3] = beta_dosage_raw[2];
}

// Model Likelihood
model{

  // Priors
  mu_alpha ~ normal(3, 10);
  sd_alpha ~ normal(0, 5);
  z_alpha  ~ normal(0,1);
  beta_dosage_raw ~ normal(0, 5);
  beta_age ~ normal(0, 3);
  sigma ~ normal(15, 10);

  // Model Likelihood
  for(n in 1:N){
    Delta_score[n] ~ normal(
      alpha[Group_idx[n]] +
      beta_dosage[Dosage[n] + 1] +
      beta_age * Z_age[n],
      sigma
    );
  }
}
























