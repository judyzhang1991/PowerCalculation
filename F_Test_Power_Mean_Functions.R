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
# sst: sum of squares between groups.
# sse: sum of squares within groups. 
# n: sample size for the control/treatment group (as we assume the two samples have the same size).
# alpha: significance level for the F-test of equality of two population means. 

## Return:
# power: calculated power for the F-test for equality of two population means. 



cal_power_mean <- function(mean1_popl, mean0_popl, var_popl, sst, sse, n, alpha){
  
  F <- (sst/(2 - 1))/(sse/(2*n - 2))
  
  mu <- (1/(2*n))*(n*mean1_popl + n*mean0_popl)
  
  delta <- n*(mean1_popl - mu)^2/var_popl + n*(mean0_popl - mu)^2/var_popl
  
  
  F_crit_up <- qf(1 - alpha, 2-1, 2*n-2, lower.tail = TRUE, log.p = FALSE)
  
  power <- pf(F_crit_up, 2-1, 2*n-2, ncp = delta, lower.tail = FALSE, log.p = FALSE)
  
  return(power)
  
  
}





## Function simulate_power_mean(): calculate powers for different values of parameters.

## Arguments:
  
# mean1popl_list: a vector containing a list of population means for different treatment groups. 
# mean0popl_list: a vector containing a list of population means for different control groups. 
# varpopl_list: a vector containing a list of sample variances for different treatment group samples. 
# sst_list: a vector containing a list of SSTs calculated for each pair of treatment v.s. control groups.
# sse_list: a vector containing a list of SSEs calculated for each pair of treatment v.s. control groups.
# n_list: a vector containing a list of sample sizes for different treatment/control group samples. 
# alpha_list: a vector containing a list of significance levels. 


## Return: 
# result_table: a tibble containing parameters and corresponding power of the F test. 



simulate_power_mean <- function(mean1popl_list, mean0popl_list, varpopl_list, sst_list, sse_list, n_list, alpha_list){
  
  power <- c()
  
  for(i in 1:length(mean1popl_list)){
    
    power[i] <- cal_power_mean(mean1popl_list[i], mean0popl_list[i], varpopl_list[i], sst_list[i], sse_list[i], n_list[i], alpha_list[i])
    
  }
  
  # Result table will provide four parameters and corresponding power: mu1 (population mean for the treatment group), mu0 (population mean for the control group), n (sample size for control/treatment group), sigma2 (population variance for control/treatment group) and power
  
  result_table <- tibble(mu1 = mean1popl_list, mu0 = mean0popl_list, sigma2 = varpopl_list, n = n_list, power = power)
  
  return(result_table)
  
  
}

