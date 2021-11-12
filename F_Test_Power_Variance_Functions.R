## F_Test_Power_Variance_Functions 
## Author: Jingyang Zhang
## Date: Nov 11th, 2021

## Purpose: contains functions that help calculating 
# power of F test for equality of population variances.




# Load Package

library(tidyverse)
library(ggpubr)





## Function cal_power_var(): calculates power for a two-sided F-test for equality of two variances. 

## Arguments: 
# var1_popl: population variance for the treatment group.
# var0_popl: population variance for the control group. 
# var1: sample variance for the treatment group.
# var0 sample variance for the control group. 
# n1: sample size for the treatment group.
# n0: sample size for the control group. 
# alpha: significance level for the F-test of equality of two population variances. 

## Return:
# power: calculated power for the F-test for equality of two variances. 


cal_power_var <- function(var1_popl, var0_popl, var1, var0, n1, n0, alpha){
  
  
  
  F_stat <- var1/var0
  
  
  phi <- var1_popl/var0_popl
  
  F_crit_low <- qf(alpha/2, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
  
  F_crit_up <- qf(1 - alpha/2, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
  
  p_low <- pf(phi*F_crit_low, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
  
  p_up <- pf(phi*F_crit_up, n1-1, n0-1, lower.tail = FALSE, log.p = FALSE)
  
  power <- p_low + p_up
  
  return(power)
  
  
}






## Function simulate_power_var(): calculate powers for different values of parameters.

## Arguments:
  
# var1popls: a vector containing a list of population variances for different treatment groups. 
# var0popls: a vector containing a list of population variances for different control groups. 
# var1s: a vector containing a list of sample variances for different treatment group samples. 
# var0s: a vector containing a list of sample variances for different control group samples. 
# n1s: a vector containing a list of sample sizes for different treatment group samples. 
# n0s: a vector containing a list of sample sizes for different treatment group samples. 
# alphas: a vector containing a list of significance levels. 


## Return: 
# result_table: a tibble containing parameters and corresponding power of the F test. 



simulate_power_var <- function(var1popls, var0popls, var1s, var0s, n1s, n0s, alphas){
  
  power <- c()
  
  for(i in 1:length(var1popls)){
    
    power[i] <- cal_power_var(var1popls[i], var0popls[i], var1s[i], var0s[i], n1s[i], n0s[i], alphas[i])
    
  }
  
  # Result table will provide three parameters and corresponding power: phi, n1, n0, and power
  if(all(n1s == n0s)){
    
    result_table <- tibble(phi = var1popls/var0popls, n = n1s, power = power)
    
  }else{
    
    result_table <- tibble(phi = var1popls/var0popls, n1 = n1s, n0 = n0s, power = power)
    
  }

  return(result_table)
  
  
}

