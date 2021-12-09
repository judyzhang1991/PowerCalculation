library(pwr)



# n = 769.1661
pwr.t.test(d = 0.1, sig.level = 0.05, type = "two.sample", alternative = "two.sided", power = 0.5
          )

# n = 769.2531
power.t.test(delta = 0.1, sd = 1, sig.level = 0.05, power = 0.5, alternative = "two.sided")
