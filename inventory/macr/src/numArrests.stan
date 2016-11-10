data {
  int<lower = 1> numGroups;
  int<lower = 1> numYears;
  
  int<lower = 0> numNonZeroYears[numGroups];
  
  matrix[numYears, numGroups] logCounts;
  vector[numGroups] medianCounts;
 
  
  vector[2] beta_eta_mean;
  vector[2] beta_eta_scale;
  
  vector[2] mu_beta_y;
  matrix[2,2] Sigma_beta_y;
  
  real eta_scale;
}
transformed data {
  vector[numYears] time;
  
  {
    real mu;
    real sigma;
    
    for (t in 1:numYears) time[t] = t;
    mu = mean(time);
    sigma = sd(time);
    time = (time - mu) / sigma;
  }
}
parameters {
  // vector<lower = -1.0, upper = 1.0>[numGroups] gamma;
  
  vector[2] beta_y[numGroups];
  vector[numGroups] eta;
  
  vector[2] beta_eta;
}
model {
  vector[2] beta_eta_st;
  
  beta_eta_st = (beta_eta - beta_eta_mean) ./ beta_eta_scale;
  for (j in 1:numGroups) {
    int T;
    real sigma_j;
    // vector[numNonZeroYears[j] - 1] mu_jt;
    vector[numNonZeroYears[j]] mu_jt;
    
    T = numNonZeroYears[j];
    
    sigma_j = exp(beta_eta_st[1] + beta_eta_st[2] * medianCounts[j] + eta[j] / eta_scale);
    
    // treat the first year as at time 0
    // mu_jt = beta_y[j][1] + gamma[j] * (logCounts[1:(T - 1),j] - beta_y[j][1]) + beta_y[j][2] * time[1:(T - 1)] / 9.5;
    mu_jt = beta_y[j][1] + beta_y[j][2] * time[1:T] / 95.0;
    
    // logCounts[2:T,j] ~ normal(mu_jt, sigma_j);
    logCounts[1:T,j] ~ normal(mu_jt, sigma_j);
  }
  
  // slight preference for positive, close to 1
  // target += beta_lpdf(0.5 * (gamma + 1.0) | 1.1, 1.05); // can skip jacobian
  
  beta_y ~ multi_student_t(3.0, mu_beta_y, Sigma_beta_y);
  
  eta ~ student_t(3.0, 0.0, 2.5);
  
  beta_eta ~ student_t(3.0, 0.0, 5.0);
}
