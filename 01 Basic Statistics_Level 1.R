#Assignment 1

#Q6 Expected number of candies
candies_count <- c(1,4,3,5,6,2) #creating vector
probability <- c(0.015,0.2,0.65,0.005,0.01,0.120)
exp_value <- sum(candies_count*probability)
exp_value

#Q7 Calculate Mean, Median, Mode, Variance, Standard Deviation, Range &     comment about the values / draw inferences, for the given dataset

data1=read.csv("E:\\Excelr\\Assignments\\01 Basic Statistics_Level 1\\Q7.csv")
View(data1)
attach(data1)

#define mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

stats <- function(v){
  cat("Mean",mean(v),"\n")
  cat("Median",median(v),"\n")
  cat("Mode",getmode(v),"\n")
  cat("Variance",var(v),"\n")
  cat("Standard Deviation",sd(v),"\n")
  range <- max(v)-min(v)
  cat("Range",range,"\n")
}

#for Points
stats(Points)

#for Score
stats(Score)

#for Weigh
stats(Weigh)

#Q8 Calculate Expected Value 
weights <- c(108, 110, 123, 134, 135, 145, 167, 187, 199)
exp_value <- mean(weights)
exp_value

#Q9 Calculate Skewness, Kurtosis & draw inferences on the following data
#Cars speed and distance 

install.packages("e1071") #install package
library(e1071) #use package

carsdata = read.csv("E:\\Excelr\\Assignments\\01 Basic Statistics_Level 1\\Q9_a.csv")
View(carsdata)
attach(carsdata)



skewness(speed)
skewness(dist)

kurtosis(speed)
kurtosis(dist)

hist(speed)
hist(dist)

#Q9b
weightdata = read.csv("E:\\Excelr\\Assignments\\01 Basic Statistics_Level 1\\Q9_b.csv")
View(weightdata)
attach(weightdata)

skewness(SP)
skewness(WT)

kurtosis(SP)
kurtosis(WT)

hist(SP)
hist(WT)

#Q11 the average weight of an adult male 

sample_size <- 2000
population_size <- 3000000
mean_wt <- 200
stddev <- 30
margin_error94 <- qnorm(0.97)*stddev/sqrt(sample_size)
margin_error96 <- qnorm(0.98)*stddev/sqrt(sample_size)
margin_error98 <- qnorm(0.99)*stddev/sqrt(sample_size)

left94 <- mean_wt-margin_error94
right94 <- mean_wt+margin_error94

left96 <- mean_wt-margin_error96
right96 <- mean_wt+margin_error96

left98 <- mean_wt-margin_error98
right98 <- mean_wt+margin_error98

cat("94% confidence",left94,right94,sep=" ")
cat("96% confidence",left96,right96,sep=" ")
cat("98% confidence",left98,right98,sep=" ")

#Q12 scores obtained by a student in tests 

scores <- c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
cat("Mean:",mean(scores))
cat("Median",median(scores))
cat("variance",var(scores))
cat("Standard Deviation",sd(scores))

getmode(scores)

hist(scores)

#Q20 Calculate the probability of MPG of Cars for the below cases.

carsdata=read.csv("E:\\Excelr\\Assignments\\01 Basic Statistics_Level 1\\Cars.csv")
View(carsdata)
attach(carsdata)

length(MPG[MPG>38])

p38 <- length(MPG[MPG>38])/length(MPG)
p38

p40 <- length(MPG[MPG<40])/length(MPG)
p40

p20_50 <- length(MPG[MPG>20&MPG<50])/length(MPG)
p20_50

#Q21 a)a)	Check whether the MPG of Cars follows Normal Distribution 
hist(MPG)
qqnorm(MPG)
qqline(MPG)
shapiro.test(MPG)

wt_ac=read.csv("E:\\Excelr\\Assignments\\01 Basic Statistics_Level 1\\wc-at.csv")
View(wt_ac)
attach(wt_ac)

hist(AT)
qqnorm(AT)
qqline(AT)
shapiro.test(AT)

hist(Waist)
qqnorm(Waist)
qqline(Waist)
shapiro.test(Waist)

#Q22 Calculate the Z scores of  90% confidence interval,94% confidence interval, 60% confidence interval 
qnorm(0.975) #90% confidence interval
qnorm(0.97) #94% confidence interval
qnorm(0.80) #60% confidence interval

#Q23 Calculate the t scores of 95% confidence interval, 96% confidence interval, 99% confidence interval for sample size of 25
qt(0.975,24) #95% confidence interval
qt(0.98,24) #96% confidence interval
qt(0.995,24) #99% confidence interval

#Q24 A Government company claims that an average light bulb lasts 
mean_population <- 270
sample_size <- 18
mean_sample <- 260
stddev_sample <- 90

tvalue <- (mean_sample-mean_population)/(stddev_sample/sqrt(sample_size))
pt(tvalue,17)
  



