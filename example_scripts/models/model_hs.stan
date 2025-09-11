data{
  int R;             // Number of regions
  int N;
  int G;
  real mu_theta;
  real sigma_theta;
  real sigma_tau;
  array[N] int<lower=1, upper=G> group_idx;
  array[N] int<lower=1, upper=R> region_idx;
  array[N] int<lower=0> y; // Observations
}
parameters{
  array[G] vector<lower=0>[R] tau;
  array[G] vector[R] theta;
  vector[N]          gamma_raw;
  vector<lower=0>[N] kappa;
}
transformed parameters{
  vector[N] gamma;

  for(i in 1:N){
    gamma[i] = theta[group_idx[i], region_idx[i]] + gamma_raw[i] * kappa[i] * tau[group_idx[i], region_idx[i]];
  }
}
model{
  for(i in 1:G){
    tau[i]   ~ normal(0, sigma_tau);
    theta[i] ~ normal(mu_theta,sigma_theta);
  }

  gamma_raw ~ std_normal();
  kappa     ~ std_normal();

  y ~ poisson_log(gamma);
}
generated quantities{
  array[N] int y_rep;

  for(i in 1:N){
      y_rep[i] = poisson_log_rng(gamma[i]); // posterior predictions
  }
}
