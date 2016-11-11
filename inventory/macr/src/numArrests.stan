data {
  int<lower = 1> J; // numGroups
  int<lower = 1> T; // numTimePoints
  
  int<lower = 0> t_j[J]; // startTimes
  int<lower = 0> T_j[J]; // endTimes
  
  matrix[T, J] y;
  
  int<lower = 0> P_y; // num individual level predictors
  int<lower = 0> P_j; // num group level predictors
  
  matrix[T,P_y] x_y;
  matrix[J,P_j] x_j;
}
parameters {
  vector[P_y] beta[J];
  vector[P_j] beta_j;
  
  vector[J] theta;
}
transformed parameters{
  vector[J] sigma;
  
  sigma = exp(x_j * beta_j + theta);
}
model {
  for (j in 1:numGroups) {
    int[T_j[j] - t_j[j] + 1] indices;
    
    indices = t_j[j]:T_j[t];
    
    y[j,indices] ~ normal(x_y[indices,] * beta[j], sigma[j]);
  }
}
