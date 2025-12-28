
// Model Variables
data{
  int <lower = 1> N; // Number of Observations
  int <lower = 1> J; // Number of Groups
  array[N] int<lower=1, upper=J> group_idx; // Group_index;
  vector[N] Memory_score; // Memory Score After Taking the Drug
}

// Model Parameters
parameters {
  // Hierarchical group means
  real theta;  // Grand mean
  vector[J] mu_raw; // Standard deviations away from the grand mean for group j
  real<lower=0> sigma_mu; // Between-group SD

  // Hierarchical group variances
  vector[J] sigma_raw; // SD in group j compared to the typical group
  real<lower=0> sigma_sigma; // Within group standard deviation
}

// Parameter Transformation
transformed parameters {
  vector[J] mu;
  vector<lower=0>[J] sigma;

  mu = theta + sigma_mu * mu_raw;
  sigma = sigma_sigma * exp(sigma_raw);
}

// Model
model{
  // Priors
  theta ~ normal(0, 2);

  mu_raw ~ normal(0, 1);
  sigma_mu ~ exponential(1);

  sigma_raw ~ normal(0, 1);
  sigma_sigma ~ exponential(1);

  // Model likelihood
  for(n in 1:N){
    Memory_score[n] ~ lognormal(mu[group_idx[n]],sigma[group_idx[n]]);
  }
}










