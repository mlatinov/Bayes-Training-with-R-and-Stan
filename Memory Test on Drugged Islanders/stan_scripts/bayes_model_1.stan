// Multilevel model: varying intercept
data {
  int<lower=1> N;          // number of observations
  int<lower=1> J;          // number of drug groups
  array[N] int<lower = 1,upper = J> Drug_group_idx;  // group index per obs
  array[N] int<lower=0, upper=2> Dosage;             // categorical dosage (0,1,2)
  vector[N] Memory;        // outcome
  vector[N] Age;           // Age
}

transformed data{
  vector[N] Z_age;
  Z_age = (Age - mean(Age)) / sd(Age);
}

parameters {
  real mu_alpha;             // population intercept
  vector[J] z_alpha;         // non-centered latent intercept per drug group
  real<lower=0> tau_alpha;   // SD for intercepts

  real beta_age;             // age effect

  vector[2] beta_dosage_raw; // effects for levels 1 and 2 (level 0 is reference)
  real<lower=0> sigma;       // residual SD
}

transformed parameters {
  vector[J] alpha;
  vector[3] beta_dosage;

  // Construct group-specific intercepts
  alpha = mu_alpha + tau_alpha * z_alpha;

  // Reference coding for dosage
  beta_dosage[1] = 0;             // reference level
  beta_dosage[2] = beta_dosage_raw[1]; // level 1 effect
  beta_dosage[3] = beta_dosage_raw[2]; // level 2 effect
}

model {
   // Prior for intercept
  mu_alpha ~ normal(60, 20);
  tau_alpha ~ normal(0, 10);
  z_alpha ~ normal(0, 1);

  // Priors for effects
  beta_age ~ normal(0, 5);
  beta_dosage ~ normal(0, 5);

  // Prior for residual SD
  sigma ~ normal(15, 10);

  // Likelihood
  for (n in 1:N) {
    Memory[n] ~ normal(
      alpha[Drug_group_idx[n]] +
      beta_dosage[Dosage[n]+1] +
      beta_age * Z_age[n],
      sigma
    );
  }
}
