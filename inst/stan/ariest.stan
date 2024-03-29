data{
  /* data inputs */
  int T;// number of times
  int A;//number of ages = 81
  int NT_obs;//number of time observations
  int ObsT[NT_obs];//times of observations
  int NAge_obs;//number of age cats
  int AgeTops[NAge_obs];//locations of age tops
  int N[NT_obs,NAge_obs];//observation denominators
  int K[NT_obs,NAge_obs];//observation numerators
  real R[A];//relative foi by age
  /* inputs for priors */
  real ari_mu; real<lower=0> ari_sig;
  real rho_mu; real<lower=0> rho_sig;
  real alpha_mu; real<lower=0> alpha_sig;
}
parameters{
  real<lower=0> lambda0;//initial foi
  real<lower=0> rho;//reversion rate
  real<lower=-1e-1,upper=1e-1> alpha;//decline rate in foi NOTE for now decline
}
transformed parameters{
  real f[T,A] = rep_array(0.0,T,A);//fraction uninfected by age
  real h[T,A] = rep_array(0.0,T,A);//h in the PDEs
  real fs[NT_obs,NAge_obs] = rep_array(0.0,NT_obs,NAge_obs);//aggregate version
  //dynamics
  //initialize h
  for(i in 1:T) {
    for(j in 1:A){
      h[i,j] = R[j] * lambda0 * exp(-alpha*(i-1)) + rho;
    }
  }
  //populate f
  //f(t,0) = 1
  for(i in 1:T){
    f[i,1] = 1.0;
  }
  //a>0
  for(j in 2:A){
    //t=0, eqm
    f[1,j] = f[1,j-1] * exp(-h[1,j]) + rho * (1-exp(-h[1,j]))/h[1,j];
    //t>0
    for(i in 2:T){
      f[i,j] = f[i-1,j-1] * exp(-h[i,j]) + rho * (1-exp(-h[i,j]))/h[i,j];
    }
  }
  //print(to_array_1d(f));
  //aggregation
  for(i in 1:NT_obs){
    for(j in 1:NAge_obs){
      if(j==1){
        fs[i,j] = mean(f[ObsT[i],1:AgeTops[j]]);
      } else {
        fs[i,j] = mean(f[ObsT[i],AgeTops[j-1]:AgeTops[j]]);
      }
    }
    //print(to_array_1d(fs));
  }
}
model{
  /* priors */
  lambda0 ~ lognormal(ari_mu,ari_sig);
  rho ~ lognormal(rho_mu,rho_sig);
  alpha ~ normal(alpha_mu,alpha_sig);
  /* likelihood */
  to_array_1d(K) ~ binomial( to_array_1d(N), to_array_1d(fs) );
  //NOTE f is uninfected -> so is K
}
