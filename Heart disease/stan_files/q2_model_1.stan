
// Model Variables
data {
  int N; // Number of observations

  // Exposure
  vector[N] Cholesterol;

  // Adjustment set
  vector[N] Age;
  array[N] int<lower = 0,upper = 1> Gender;

  // Outcome
  array[N] int<lower = 0,upper = 1> Diagnosis;

}

// Model Parameters
parameters {
  real beta_0;
  real beta_cholesterol;
  real beta_age;
  real beta_gender;
}

// Model Specification
model {

  // Priors
  beta_cholesterol ~ normal(0,1);
  beta_gender ~ normal(0,1);
  beta_age ~ normal(0,1);
  beta_0 ~ normal(0,1);

  // Model Likelihood
  for(n in 1:N){
    Diagnosis[n] ~ bernoulli_logit(
      beta_0 +
      beta_cholesterol * Cholesterol[n] +
      beta_age * Age[n] +
      beta_gender * Gender[n]
    );
  }
}

// Additional Model Output
generated quantities{

  // Posterior Predictive Distribution
  array[N] int y_rep; // Replicated Data

  // Replication Loop
  for(n in 1:N){
    real p;            // Replicated Prob

    // Calculate p
    p = inv_logit(
      beta_0 +
      beta_cholesterol * Cholesterol[n] +
      beta_age * Age[n] +
      beta_gender * Gender[n]
    );
    // Sample from the p
    y_rep[n] = bernoulli_rng(p);
  }
}

