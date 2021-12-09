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
# n1: sample size for the treatment group.
# n0: sample size for the control group. 
# alpha: significance level for the F-test of equality of two population variances. 

## Return:
# power: calculated power for the F-test for equality of two variances. 


cal_power_var <- function(var1_popl, var0_popl, n1, n0, alpha){
  
  
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
# n1s: a vector containing a list of sample sizes for different treatment group samples. 
# n0s: a vector containing a list of sample sizes for different treatment group samples. 
# alphas: a vector containing a list of significance levels. 


## Return: 
# result_table: a tibble containing parameters and corresponding power of the F test. 



simulate_power_var <- function(var1popls, var0popls, n1s, n0s, alphas){
  
  power <- c()
  
  for(i in 1:length(var1popls)){
    
    power[i] <- cal_power_var(var1popls[i], var0popls[i], n1s[i], n0s[i], alphas[i])
    
  }
  
  # Result table will provide three parameters and corresponding power: phi, n1, n0, and power
  if(all(n1s == n0s)){
    
    result_table <- tibble(phi = var1popls/var0popls, n = n1s, power = power)
    
  }else{
    
    result_table <- tibble(phi = var1popls/var0popls, n1 = n1s, n0 = n0s, power = power)
    
  }

  return(result_table)
  
  
}




## Function cal_effectSize_var_num(): calculates effect size phi for a two-sided F-test for equality of two variances numerically.

## Arguments: 
# n1: sample size for the treatment group.
# n0: sample size for the control group. 
# power: power of the two-sided F test for equality of two population variances.
# alpha: significance level for the F-test of equality of two population variances. 

## Return:
# phi = $\frac{\sigma_1^2}{\sigma_0^2}$: calculated effect size for the F-test for equality of two variances given significance level alpha and power. 


cal_effectSize_var_num <- function(n1, n0, power, alpha){
  
  
  F_crit_low <- qf(alpha/2, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
  
  F_crit_up <- qf(1 - alpha/2, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
  
  phi <- 1
  
  p_low <- pf(phi*F_crit_low, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
  
  p_up <- pf(phi*F_crit_up, n1-1, n0-1, lower.tail = FALSE, log.p = FALSE)
  
  power_cal <- p_low + p_up
  
  
  # Limit error within 0.05 (the difference between the calculated power and the given power is no larger than 0.01)
  while(abs(power_cal - power) > 0.05){
    
    # If calculated power is greater than the given power, we will decrease the difference between 1 and current value of phi.
    if(power_cal > power){
      
      
      phi <- ifelse(phi > 1, phi - 0.01, phi + 0.01) 
      
    }
    
    # If the calculated power is less than the given power, we will increase the difference between 1 and current value of phi. 
    else{
      
      phi <- ifelse(phi > 1, phi + 0.01, phi - 0.01)
    }
    
    p_low <- pf(phi*F_crit_low, n1-1, n0-1, lower.tail = TRUE, log.p = FALSE)
    
    p_up <- pf(phi*F_crit_up, n1-1, n0-1, lower.tail = FALSE, log.p = FALSE)
    
    power_cal <- p_low + p_up
    
    
    
    
  }
  print(paste("power:", power_cal))
 
  
  return(phi)
  
  
}




## Function cal_effectSize_var(): calculates effect size phi for a two-sided F-test for equality of two variances analytically.

## Arguments: 
# n1: sample size for the treatment group.
# n0: sample size for the control group. 
# power: power of the two-sided F test for equality of two population variances.
# alpha: significance level for the F-test of equality of two population variances. 

## Return:
# phi = $\frac{\sigma_1^2}{\sigma_0^2}$: calculated effect size for the F-test for equality of two variances given significance level alpha and power. 


cal_effectSize_var <- function(n1, n0, power, alpha){
  
  

  
  return(phi)
  
  
}
