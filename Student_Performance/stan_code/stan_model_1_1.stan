
// Declaration of data
data {
  int<lower=0> N; // Number of observations
  vector[N] Y;    // Test Results
  vector[N] X;    // Previous Scores
}

// Model Parameters
parameters {
  real alpha;                 // Baseline parameter
  real beta_1;               // Beta 1 coefficient for the effect of the previous scores
  real < lower = 0 > sigma; // SD of Test results
}

// Transformations
transformed parameters{
  vector[N] mu_test_results = alpha + beta_1 * X; // Declare mu_test_results
}

// Model Likehood
model {
  Y ~ normal(mu_test_results,sigma);
}

