data {
  int<lower = 1> numGroups;
  int<lower = 1> numYears;
  
  int<lower = 0> numNonZeroYears[numGroups];
  
  matrix[numYears, numGroups] logCounts;
  vector[numGroups] medianCounts;
}
transformed data {
  vector[numYears - 1] time;
  
  for (t in 1:(numYears - 1)) time[t] = t;
}
parameters {
  vector<lower = -1.0, upper = 1.0>[numGroups] gamma;
  vector[numGroups] beta_y;
  
  vector[numGroups] eta;
  
  real alpha_eta;
  real beta_eta;
}
model {
  for (j in 1:numGroups) {
    int T;
    real sigma;
    
    T = numNonZeroYears[j];
    
    sigma = exp((alpha_eta - 7.0) / 12.0 + (beta_eta - 0.5) * medianCounts[j] / 7.5 + eta[j] / 2.0);
    logCounts[2:T,j] ~ normal(gamma[j] * logCounts[1:(T - 1),j] + beta_y[j] * time[1:(T - 1)] / 95.0, sigma);
  }
  
  // slight preference for positive, close to 1
  target += beta_lpdf(0.5 * (gamma + 1.0) | 1.1, 1.05); // can skip jacobian
  
  beta_y ~ student_t(3.0, 0.0, 5.0);
  
  eta ~ student_t(3.0, 0.0, 2.5);
  
  beta_eta ~ student_t(3.0, 0.0, 5.0);
  alpha_eta ~ student_t(3.0, 0.0, 5.0);
}
