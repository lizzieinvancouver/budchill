
// budchill experiment analysis
data {
  int<lower=0> N;
  int<lower=0> n_sp;
  int<lower=1, upper=n_sp> sp[N];
  vector[N] lday;
  vector[N] chill1; 
  vector[N] chill2; 
  vector[N] chill4;
  vector[N] chill8;
  vector[N] time2; 
  vector[N] time3;
}

parameters {
  vector[n_sp] a_sp;
  vector[n_sp] b_chill1;
  vector[n_sp] b_chill2;
  vector[n_sp] b_chill4;
  vector[n_sp] b_chill8;
  vector[n_sp] b_time2;
  vector[n_sp] b_time3;  
 
  real mu_b_chill1;
  real mu_b_chill2;
  real mu_b_chill4;
  real mu_b_chill8;
  real mu_b_time2;
  real mu_b_time3;
    
  real<lower=0> sigma_b_chill1;
  real<lower=0> sigma_b_chill2;
  real<lower=0> sigma_b_chill4;
  real<lower=0> sigma_b_chill8;
  real<lower=0> sigma_b_time1;
  real<lower=0> sigma_b_time2;
  
  real<lower=0> sigma_y; 
  }


transformed parameters {
	vector[N] y_hat;

	for(i in 1:N){
		y_hat[i] <- a[sp[i]] + b_chill1[sp[i]] * chill1[i] + b_chill2[sp[i]] * chill2[i]+ b_chill4[sp[i]] * chill4[i]+ b_chill8[sp[i]] * chill8[i] +
						b_time1[sp[i]] * time1[i] + b_time2[sp[i]] * time2[i]
		;
		
		}
	
}

model {
	// Priors. Make them flat
	mu_b_chill1 ~ normal(0, 35);
	mu_b_chill2 ~ normal(0, 35);
	mu_b_chill3 ~ normal(0, 35);
	mu_b_chill3 ~ normal(0, 35);
	mu_b_time1 ~ normal(0, 35);
	mu_b_time2 ~ normal(0, 35);		
	
	sigma_b_chill1 ~ normal(0, 10);
	sigma_b_chill2 ~ normal(0, 10);
	sigma_b_chill3 ~ normal(0, 10);
	sigma_b_chill4 ~ normal(0, 10);
	sigma_b_time1 ~ normal(0, 10);
	sigma_b_time2 ~ normal(0, 10);		

	
	b_chill1 ~ normal(mu_b_chill1, sigma_b_chill1);
	b_chill2 ~ normal(mu_b_chill2, sigma_b_chill2);
	b_chill3 ~ normal(mu_b_chill3, sigma_b_chill3);
	b_chill4 ~ normal(mu_b_chill4, sigma_b_chill4);
	b_time1 ~ normal(mu_b_time1, sigma_b_time1);
	b_time2 ~ normal(mu_b_time2, sigma_b_time2);		

	lday ~ normal(y_hat, sigma_y);
}