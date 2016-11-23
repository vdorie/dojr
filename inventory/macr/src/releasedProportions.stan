data {
  int<lower = 1> J; // numGroups;
  int<lower = 1> T; // numYears;
  
  int<lower = 0> P;       // num predictors in response model, constant across groups
  int<lower = 0> P_j;     // num predictors in response model, vary by group
    
  int indices[T,J];
  int numIndices[J];
  
  int<lower = 0> n[T, J]; // num arrested
  int<lower = 0> y[T, J]; // num released
  
  matrix[T,P]   x;   // predictors in response model, constant across groups
  matrix[T,P_j] x_j; // predictors in response model, varying across groups
  
  
  // hyperparameters
  
  // prior on population fixed effects
  real<lower = 0.0> nu_beta;
  vector[P] mu_beta;
  vector[P] sigma_beta;
  
  // prior on random effects covariance
  real<lower = 0.0> nu_L_sigma_beta_j;
  vector<lower = 0.0>[P_j] sigma_L_sigma_beta_j;
  real<lower = 0.0> nu_L_Omega_beta_j;
}
/* transformed data {
  vector[P_j] mu_beta_j;
  
  for (p in 1:P_j) mu_beta_j[p] = 0.0;
} */
parameters {
  vector[P] beta;
  matrix[P_j, J] z;
  
  cholesky_factor_corr[P_j] L_Omega_beta_j;
  vector<lower = 0.0>[P_j] L_sigma_beta_j;
}
model {
  matrix[P_j,J] beta_j;
  matrix[P_j,P_j] L_Sigma_beta_j;
  
  L_Sigma_beta_j = diag_pre_multiply(L_sigma_beta_j, L_Omega_beta_j);
  beta_j = L_Sigma_beta_j * z;
  
  for (j in 1:J) {
    if (numIndices[j] < T) {
      int indices_j[numIndices[j]];
      indices_j = indices[1:numIndices[j],j];
    
      y[indices_j,j] ~ binomial_logit(n[indices_j,j], x[indices_j,] * beta + x_j[indices_j,] * beta_j[,j]);
    } else {
      y[,j] ~ binomial_logit(n[,j], x * beta + x_j * beta_j[j]);
    }
  }
  
  // apply t priors to "fixed effects"
  beta ~ student_t(nu_beta, mu_beta, sigma_beta);
  
  
  // hierarchically model group coefficients (random effects)
  // give them a normal distribution with a mean of 0 and an unknown covariance
  
  L_sigma_beta_j ~ student_t(nu_L_sigma_beta_j, 0.0, sigma_L_sigma_beta_j);
  L_Omega_beta_j ~ lkj_corr_cholesky(nu_L_Omega_beta_j);
  
  to_vector(z) ~ normal(0.0, 1.0);
}

