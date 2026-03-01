
data {
  
  int<lower=0> N;
  array[N] int choiceR;
  matrix[N,3] prizes;
  
  matrix[N,3] probL, probR;
  
}

transformed data {
  
  matrix[N,3] dprob = probR-probL;
  
}

parameters {
  
  real r;
  real<lower=0> lambda;
  
}


model {

  // prior
  r ~ normal(0,1);
  lambda ~ lognormal(0, 1);
  
  
  // likelihood
  matrix[N,3] U = (1.0-exp(-r*prizes))/r;
  // contextual utility normalization
  U = U./rep_matrix(U[,3]-U[,1],3);

  vector[N] lDU = lambda*(dprob.*U)*rep_vector(1.0,3);
  choiceR ~ bernoulli_logit(lDU);

}

