
// Declare Variables
data {
  int <lower = 0> N; // Number of Observations
  vector[N] Previous_score; // Previous Score
  vector[N] Hours_study;    // Hours study for the test
  vector[N] Test_results;   // Target Variable
}

// Declare Parameters
parameters {
  real alpha;              // Baseline
  real b_prevuous_score;   // Coef for previous score
  real b_hours_study;      // Coef for hours study
  real <lower = 0> sigma;  // SD for the Test results
}

// Transformations
transformed parameters{
  // Declare mu for the model
  vector[N] mu_test_results =
  alpha +
  b_prevuous_score * Previous_score +
  b_hours_study * Hours_study;
}

// Model Likehood
model {
  Test_results ~ normal(mu_test_results, sigma);
}

