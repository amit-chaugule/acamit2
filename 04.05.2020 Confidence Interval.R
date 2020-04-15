#calculate Confidence interval Z-Value

#given
mean1 <- 1990
sigma1 <- 2500 # sigma for population
sample1 <- 140

#calculate error
Zerror <- qnorm(0.975)*sigma1/sqrt(sample1)
Zleft <- mean1-Zerror
Zright <- mean1+Zerror
Zleft
Zright

#calculate Confidence interval - T-Value
  #when population std dev is not given

#given
mean2 <- 1990
sigma2 <- 2833 #sigma for sample
sample2 <- 140

#calculate error
Terror <- qt(0.975,df=sample2-1)*sigma2/sqrt(sample2)
Tleft <- mean1-Terror
Tright <- mean1+Terror
Tleft
Tright
