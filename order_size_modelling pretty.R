#-----------------------
#Visualisation and exploration
#-----------------------

x <- datalevels[[j]][[i]][[1]]$Size#[limit_indices[[j]][[i]]] 
# Uncomment the above comment to use only limit orders - can change to only market or cancellation orders as well

# Exploration
source("order_size_exploration.R")

#----------------------------
# POWER LAW FITTING
#----------------------------

# Install and set up the poweRlaw package
install.packages("poweRlaw")
library(poweRlaw)

# Create function to estimate parameters
parameter_estimation_pl <- function(x){
  data_pl <- displ$new(x) # displ creates a DIScrete Power Law object from the data
  
  #estimate the parameters: the power alpha and the lower bound of power law behaviour xmin
  est <- estimate_xmin(data_pl)
  
  # Set the power law object to have these parameters
  data_pl$xmin <- est$xmin 
  data_pl$pars <- est$pars
  
  return(data_pl)
}

# For all data
  data_all_pl <- parameter_estimation_pl(x)
  
  # # Note x has length approx 1 million for MSFT, and so takes a very long time to run. Instead of x can use
  # sampledx <- sample(x, min(10000,length(x)), replace = T) # sample ten thousand with replacement
  
  # Perform the bootstrap with 2500 simulations running on 24 threads
  btstrap <- bootstrap_p(data_all_pl, no_of_sims = 2500, threads = 24)


# For only multiples of 100
  # Extract multiples of 100, and write e.g. 300 as 3
  y <- x[which(x%%100==0)] / 100
  data_100s_pl <- parameter_estimation_pl(y)
  # Perform the bootstrap with 2500 simulations on 24 threads
  bs <- bootstrap_p(data_100s_pl, no_of_sims = 2500, threads = 24 )
  
  
#compare to a lognormal
  # Create discrete lognormal object from the data
  data_ln <- dislnorm$new(x)
  
  # Assign the same x_min as the parameter for the power law to ensure a correct comparison
  data_ln$xmin <- est$xmin
  
  # Estimate the other parameter for lognormal object, and set it
  estln <- estimate_pars(data_ln)
  data_ln$setPars(estln)
  
  # Compare the power law object to the lognormal object
  comp <- compare_distributions(data_all_pl, data_ln)
  list(comp$test_statistic, comp$p_two_sided)

  
#compare to an exponential
  # Create discrete exponential object from the data
  data_exp <- disexp$new(x)
  
  # Assign the same x_min as the parameter for the power law
  data_exp$xmin <- est$xmin
  
  # Estimate the other parameter for the exponential object, and set it
  estexp <- estimate_pars(data_exp)
  data_exp$setPars(estexp)
  
  # Compare the power law object to the exponential object
  comp2 <- compare_distributions(data_all_pl, data_exp)
  list(comp2$test_statistic, comp2$p_two_sided)