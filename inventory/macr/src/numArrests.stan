data {
  int<lower = 1> J; // numGroups
  int<lower = 1> T; // numTimePoints
  
  int indices[T,J]; // which years to include in model
  int numIndices[J];
  
  int<lower = 0> P;       // num predictors in response model, constant across groups
  int<lower = 0> P_j;     // num predictors in response model, vary by group
  int<lower = 0> P_sigma; // num predictors in residual noise model, constant across groups
  
  matrix[T,J] y;
  
  matrix[T,P]       x;        // predictors in response model, constant across groups
  matrix[T,P_j]     x_j;      // predictors in response model, varying across groups
  matrix[J,P_sigma] x_sigma;  // predictors in residual noise model
  vector[J] w;                // one for each group, to be used in 
  
  // hyperparameters
  
  // prior on population fixed effects
  real<lower = 0.0> nu_beta;
  vector[P] mu_beta;
  vector[P] sigma_beta;
  
  // prior on residual variance fixed effects
  real<lower = 0.0> nu_beta_sigma;
  vector[P_sigma] mu_beta_sigma;
  vector<lower = 0.0>[P_sigma] sigma_beta_sigma;
  
  // prior on random effects covariance
  real<lower = 0.0> nu_L_sigma_theta;
  vector<lower = 0.0>[P_j + 1] sigma_L_sigma_theta;
  real<lower = 0.0> nu_L_Omega_theta;
}
parameters {
  vector[P] beta;
  matrix[P_j + 1,J] z;
  vector[P_sigma] beta_sigma;
  
  cholesky_factor_corr[P_j + 1] L_Omega_theta;
  vector<lower = 0>[P_j + 1] L_sigma_theta;
}
model {
  matrix[P_j + 1,J] theta;
  matrix[P_j + 1,P_j + 1] L_Sigma_theta;
  
  L_Sigma_theta = diag_pre_multiply(L_sigma_theta, L_Omega_theta);
  theta = L_Sigma_theta * z;
  
  for (j in 1:J) {
    vector[P_j] beta_j;
    real eta;
    real sigma;
    
    beta_j = theta[1:P_j,j];
    eta    = theta[P_j + 1,j];
    
    sigma = exp(x_sigma[j,] * beta_sigma + eta);
    
    if (numIndices[j] < T) {
      int indices_j[numIndices[j]];
      indices_j = indices[1:numIndices[j],j];
    
      y[indices_j,j] ~ normal(x[indices_j,] * beta / w[j] + x_j[indices_j,] * beta_j, sigma);
    } else {
      y[,j] ~ normal(x * beta / w[j] + x_j * beta_j, sigma);
    }
  }
  
  // apply t priors to "fixed effects"
  beta ~ student_t(nu_beta, mu_beta, sigma_beta);
  
  beta_sigma ~ student_t(nu_beta_sigma, mu_beta_sigma, sigma_beta_sigma);
  
  
  // hierarchically model group coefficients (random effects)
  // give them a normal distribution with a mean of 0 and an unknown covariance
  
  L_sigma_theta ~ student_t(nu_L_sigma_theta, 0.0, sigma_L_sigma_theta);
  L_Omega_theta ~ lkj_corr_cholesky(nu_L_Omega_theta);
  
  to_vector(z) ~ normal(0.0, 1.0);
}

