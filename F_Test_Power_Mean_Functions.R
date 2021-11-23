## F_Test_Power_Mean_Functions 
## Author: Jingyang Zhang
## Date: Nov 11th, 2021

## Purpose: contains functions that help calculating 
# power of F test for equality of population means.




# Load Package

library(tidyverse)
library(ggpubr)



## Function cal_power_mean(): calculates power for a two-sided F-test for equality of two population means. 

## Arguments: 
# mean1_popl: population mean for the treatment group.
# mean0_popl: population mean for the control group. 
# var_popl: population variance for the control/treatment group (as we assume the two populations have the same variance).
# n: sample size for the control/treatment group (as we assume the two samples have the same size).
# alpha: significance level for the F-test of equality of two population means. 

## Return:
# power: calculated power for the F-test for equality of two population means. 



cal_power_mean <- function(mean1_popl, mean0_popl, var_popl, n, alpha){
  
  
  mu <- (1/(2*n))*(n*mean1_popl + n*mean0_popl)
  
  delta <- n*(mean1_popl - mu)^2/var_popl + n*(mean0_popl - mu)^2/var_popl
  
  
  F_crit_up <- qf(1 - alpha, 2-1, 2*n-2, lower.tail = TRUE, log.p = FALSE)
  
  power <- pf(F_crit_up, 2-1, 2*n-2, ncp = delta, lower.tail = FALSE, log.p = FALSE)
  
  return(power)
  
  
}





## Function simulate_power_mean(): calculate powers for different values of parameters.

## Arguments:
  
# mean1popls: a vector containing a list of population means for different treatment groups. 
# mean0popls: a vector containing a list of population means for different control groups. 
# varpopls: a vector containing a list of sample variances for different treatment group samples. 
# ns: a vector containing a list of sample sizes for different treatment/control group samples. 
# alphas: a vector containing a list of significance levels. 


## Return: 
# result_table: a tibble containing parameters and corresponding power of the F test. 



simulate_power_mean <- function(mean1popls, mean0popls, varpopls, ns, alphas){
  
  powers <- c()
  
  for(i in 1:length(mean1popls)){
    
    powers[i] <- cal_power_mean(mean1popls[i], mean0popls[i], varpopls[i], ns[i], alphas[i])
    
  }
  
  # Result table will provide four parameters and corresponding power: mu1 (population mean for the treatment group), mu0 (population mean for the control group), n (sample size for control/treatment group), sigma2 (population variance for control/treatment group) and power
  
  result_table <- tibble(mu1 = mean1popls, mu0 = mean0popls, sigma2 = varpopls, n = ns, power = powers)
  
  return(result_table)
  
  
}




## Function cal_sampleSize_mean(): calculate sample size required for a given power of the F-test for equality of two population means.

## Arguments:

# mean1popls: a vector containing a list of population means for different treatment groups. 
# mean0popls: a vector containing a list of population means for different control groups. 
# varpopls: a vector containing a list of sample variances for different treatment group samples. 
# ns: a vector containing a list of sample sizes for different treatment/control group samples. 
# alphas: a vector containing a list of significance levels. 


## Return: 
# result_table: a tibble containing parameters and corresponding power of the F test. 



simulate_power_mean <- function(mean1popls, mean0popls, varpopls, ns, alphas){
  
  powers <- c()
  
  for(i in 1:length(mean1popls)){
    
    powers[i] <- cal_power_mean(mean1popls[i], mean0popls[i], varpopls[i], ns[i], alphas[i])
    
  }
  
  # Result table will provide four parameters and corresponding power: mu1 (population mean for the treatment group), mu0 (population mean for the control group), n (sample size for control/treatment group), sigma2 (population variance for control/treatment group) and power
  
  result_table <- tibble(mu1 = mean1popls, mu0 = mean0popls, sigma2 = varpopls, n = ns, power = powers)
  
  return(result_table)
  
  
}



## Function cal_sampleSize_var(): calculates the minimum sample size n per group required for the F-test for equality of two population means to have a certain given power. 

### Note: the F-test for equality of two population means is equivalent to the two-sample t test for population means.

## Arguments: 
# delta: effect size for the F-test for equality of two population means.
# power: power of the two-sided F test for equality of two population means.
# alpha: significance level for the F-test of equality of two population means. 

## Return:
# n: minimum sample size per group required for the F-test for equality of two population means to reach the given power. 


cal_sampleSize_mean <- function(delta, power, alpha){
  
  
  
  n_min <- round(as.numeric(pwr.t.test(d=delta, sig.level=alpha, power=power, type="two.sample", alternative="two.sided")$n))
  
  return(n_min)
}










