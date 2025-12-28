
// Model Variables
data{
  int <lower = 1> N; // Number of Observations
  int <lower = 2> J; // Number of Groups
  array[N] int<lower = 1,upper = J> Group_idx; // Group Index
  vector[N] Memory_score; // Outcome Variable
}

// Model Parameters
parameters{

  // Multilevel Means
  real theta;        // Joint Population Mean
  vector[J] mu_raw;  // SD away from the theta for each group J
  real sigma_mu;     // Variation in groups relative to theta

  // Multilevel SDs
  vector[J] sigma_raw;          // SD in group J relative to the typical group
  real <lower = 0> sigma_sigma; // In group variation
}

// Parameter Transformations
transformed parameters{
  vector[J] mu;
  vector<lower=0>[J] sigma;

  mu = theta + mu_raw * sigma_mu;
  sigma = sigma_sigma + exp(sigma_raw);
}

// Model Specification
model{

  // Priors
  theta ~ normal(0, 2);

  mu_raw ~ normal(0, 1);
  sigma_mu ~ exponential(1);

  sigma_raw ~ normal(0, 1);
  sigma_sigma ~ exponential(1);

  // Model Likekihood
  for(n in 1:N){
    Memory_score[n] ~ normal(mu[Group_idx[n]],sigma[Group_idx[n]]);
  }
}














