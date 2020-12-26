# QUESTION 1
# Part a
# in report

# Part b
# Set up the n and theta for comparisons
n = c(1,2,3,4,10,30)
theta = c(1,5,50,100)

# Index for looping through the values of n and theta
indexN = 1
indexT = 1

replication_runs = 1000 # We want 1000 replications 
combinations = length(n) * length(theta) # there are 6 * 4 combinations of n,theta
mle_error = seq(0,1, length.out = combinations)
mom_error = seq(0,1, length.out = combinations)

indexL = 1
repeat {
  if(indexL > combinations) { # We are done when we loop through all the combinations
    break
  }
  # We do 1000 samples of a combination of (n,theta) from uniform distribution
  samples = replicate(replication_runs,runif(n[indexN],0,theta[indexT]))
  thetaMLE = max(samples) # We compute the Maximum likelihood estimator for those samples
  thetaMOM = mean(samples) * 2 # We compute the Method of Moments estimator for theta of those samples
  
  # Save the squared errors for the two estimators
  mle_error[indexL] = (thetaMLE - theta[indexT])^2
  mom_error[indexL] = (thetaMOM - theta[indexT])^2
  
  # checking for all the combinations are exhausted
  if(indexT == 4) {
    indexN  = indexN + 1
    indexT = 1
  } else {
    indexT = indexT + 1
  }
  indexL = indexL + 1
}

# Part c
# show the results in a histogram
hist(mle_error, ylim = c(0,combinations + 1))
hist(mom_error, ylim = c(0,combinations + 1))

# Part d
# in report


# QUESTION 2
n = 5
MLE_samples = c(21.72, 14.65, 50.42, 28.78, 11.23)

# Part a
estimated_theta = n / sum(log(MLE_samples))

# Part b
log_likelihood_function <- function(par) {
  n * log(par) - (par * sum(log(MLE_samples))) - sum(log(MLE_samples)) 
}

# Part c
optim(par<-0.2, fn=log_likelihood_function, control = list(fnscale=-1))

# Part d
# Standard error of theta
seMle <- sqrt((0.3234375 * 0.3234375)/ 5)
# confidence interval
c(0.3234375 - (1.96 * seMle),0.3234375 + (1.96 * seMle))