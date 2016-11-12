data {
  int<lower = 1> J; // numGroups
  int<lower = 1> T; // numTimePoints
  
  int<lower = 0> start_j[J];
  int<lower = 0> end_j[J];
  
  matrix[T, J] y;
  
  int<lower = 0> P_y; // num individual level predictors
  int<lower = 0> P_j; // num group level predictors
  
  matrix[T,P_y] x_y;
  matrix[J,P_j] x_j;
  
  real<lower = 0.0> nu_theta;
  vector[P_y + 1] mu_theta;
  matrix[P_y + 1, P_y + 1] Sigma_theta;
}
transformed data {
  int T_j[J];
  
  for (j in 1:J) T_j[j] = end_j[j] - start_j[j] + 1;
}
parameters {
  vector[P_y + 1] theta[J];
  vector[P_j] beta_j;
}
model {
  for (j in 1:J) {
    vector[P_y] beta;
    real eta;
    real sigma;
    int indices[T_j[j]];
    
    beta = theta[j][1:P_y];
    eta = theta[j][P_y + 1];
    
    sigma = exp(x_j[j,] * beta_j + eta);
    
    for (t in 1:T_j[j]) indices[t] = start_j[j] + t - 1;
    
    y[indices,j] ~ normal(x_y[indices,] * beta, sigma);
  }
  
  theta ~ multi_student_t(nu_theta, mu_theta, Sigma_theta);
}

