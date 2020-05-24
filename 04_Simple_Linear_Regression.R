###1) Calories_consumed-> predict weight gained using calories consumed

#read csv file
calories_data <- read.csv("E:/Excelr/Assignments/04_Simple_Linear_Regression/calories_consumed.csv")
#view data
View(calories_data)
#plot to visualize the correlation
plot(calories_data$Calories.Consumed,calories_data$Weight.gained..grams.)

#graphical visualization
install.packages("lattice")
library("lattice")
dotplot(calories_data$Weight.gained..grams.,main="Dot Plot of Weight Gained")
dotplot(calories_data$Calories.Consumed,main ="Dot Plot of Calories Consumed")
boxplot(calories_data$Weight.gained..grams.,col = "dodgerblue4")
boxplot(calories_data$Calories.Consumed,col="red",horizontal=T)
hist(calories_data$Weight.gained..grams.)
hist(calories_data$Calories.Consumed)

hist(calories_data$Weight.gained..grams.,prob=TRUE)
lines(density(calories_data$Weight.gained..grams.))
lines(density(calories_data$Weight.gained..grams.,adjust = 2),lty="dotted")

#check normality
qqnorm(calories_data$Weight.gained..grams.)
qqline(calories_data$Weight.gained..grams.)

qqnorm(calories_data$Calories.Consumed)
qqline(calories_data$Calories.Consumed)

#scatter plot
plot(calories_data$Calories.Consumed,calories_data$Weight.gained..grams.,
     main="Scatter Plot",
     col="red",col.main="blue",col.lab="orange",
     xlab = "Calories Consumed",
     ylab = "Weight Gained",
     pch=35) #pch for shape


#find correlation
cor(calories_data$Weight.gained..grams.,calories_data$Calories.Consumed)
#perform linear regression
linreg=lm(calories_data$Weight.gained..grams.~calories_data$Calories.Consumed,data = calories_data)
#print observations/exploratory data analysis
summary(linreg)
#find confidence interval
confint(linreg,level=0.95)
#find predicted interval
pred <- predict(linreg,interval="predict")
View(pred)
#convert pred to data frame
pred <- as.data.frame(pred)
#find correlation of fitted value with independent variable
cor(pred$fit,calories_data$Calories.Consumed)
#plot linear regression line on scatter plot
abline(linreg)

################################################################
###2) Delivery_time -> Predict delivery time using sorting time 
delivery_data=read.csv("E:/Excelr/Assignments/04_Simple_Linear_Regression/delivery_time.csv")
View(delivery_data)
attach(delivery_data)
#plot graph
plot(Sorting.Time,Delivery.Time,
     main="Deliver Time vs Sorting Time: Scatter Plot",
     col="red",col.main="mediumblue",col.lab="darkblue",
     xlab = "Sorting.Time",
     ylab = "Delivery.Time",
     pch=35) #pch for shape
#checking for outliers
boxplot(Sorting.Time)
boxplot(Delivery.Time)

hist(Delivery.Time)
hist(Sorting.Time)

#check for normality
qqnorm(Delivery.Time)
qqline(Delivery.Time)

qqnorm(Sorting.Time)
qqline(Sorting.Time)

#find correlation
cor(Delivery.Time,Sorting.Time)

#perform linear regression
linreg2 = lm(Delivery.Time~Sorting.Time, data=delivery_data)
summary(linreg2)
abline(linreg2)

#confidence interval
confint(linreg2,level = 0.95)

#predicted values
pred2 <- predict(linreg2,interval = "predict")
View(pred2)
pred2 <- as.data.frame(pred2)
cor(pred2$fit,Sorting.Time)

#---using sqrt transform
#perform linear regression
linreg2 = lm(sqrt(Delivery.Time)~sqrt(Sorting.Time), data=delivery_data)
summary(linreg2)
plot(sqrt(Delivery.Time),sqrt(Sorting.Time))
abline(linreg2)

#confidence interval
confint(linreg2,level = 0.95)

#predicted values
pred2 <- predict(linreg2,interval = "predict")
View(pred2*pred2)
pred2 <- as.data.frame(pred2)
cor(pred2$fit,Sorting.Time)

#---using log transform
#perform linear regression
linreg2 = lm(log(Delivery.Time)~log(Sorting.Time), data=delivery_data)
summary(linreg2)
#plot(log(Delivery.Time),log(Sorting.Time)
abline(linreg2)

#confidence interval
confint(linreg2,level = 0.95)

#predicted values
pred2 <- predict(linreg2,interval = "predict")
View(exp(pred2))
pred2 <- as.data.frame(pred2)
cor(pred2$fit,Sorting.Time)

#found best R^2 value using log transform

#############################################################
###3) Emp_data -> Build a prediction model for Churn_out_rate 

#read csv
emp_data <- read.csv("E:/Excelr/Assignments/04_Simple_Linear_Regression/emp_data.csv")
View(emp_data)
#change name of column from Salary_hike to Year
emp_data <- setNames(emp_data,c("Year","Churn_Rate"))
attach(emp_data)

#plot graph
plot(Year,Churn_Rate,
     main="Year vs Churn Out Rate: Scatter Plot",
     col="red",col.main="mediumblue",col.lab="darkblue",
     xlab = "Year",
     ylab = "Churn Out Rate",
     pch=35) #pch for shape

#checking Outiers
boxplot(Year)
boxplot(Churn_Rate)

hist(Year)
hist(Churn_Rate)

#check for normality
qqnorm(Year)
qqline(Year)

qqnorm(Churn_Rate)
qqline(Churn_Rate)

#find correlation
cor(Year,Churn_Rate)

#perform linear regression
linreg3=lm(Churn_Rate~Year,data = emp_data)
summary(linreg3)

#confidence Interval
confint(linreg3,level=0.95)

#predicted values
pred3 <- predict(linreg3,interval = "predict")
pred3 <- as.data.frame(pred3)
View(pred3)
cor(pred3$fit,Year)

#-----using sqrt transform
#perform linear regression
linreg3=lm(sqrt(Churn_Rate)~sqrt(Year),data = emp_data)
summary(linreg3)

#confidence Interval
confint(linreg3,level=0.95)

#predicted values
pred3 <- predict(linreg3,interval = "predict")
pred3 <- as.data.frame(pred3)
View(pred3$fit*pred3$fit)
cor(pred3$fit,Year)

#-----using log transform
#perform linear regression
linreg3=lm(log(Churn_Rate)~log(Year),data = emp_data)
summary(linreg3)

#confidence Interval
confint(linreg3,level=0.95)

#predicted values
pred3 <- predict(linreg3,interval = "predict")
pred3 <- as.data.frame(pred3)
View(exp(pred3))
cor(pred3$fit,Year)

#found best R^2 value using log transform

################################################################
#4) Salary_hike -> Build a prediction model for Salary_hike
#read csv
salary_data <- read.csv("E:/Excelr/Assignments/04_Simple_Linear_Regression/Salary_Data.csv")
View(salary_data)
attach(salary_data)

#plot graph
plot(YearsExperience,Salary,
     main="Salary vs Years Experience: Scatter Plot",
     col="red",col.main="mediumblue",col.lab="darkblue",
     xlab = "Years Experience",
     ylab = "Salary",
     pch=35) #pch for shape

#checking for outliers
boxplot(YearsExperience)
boxplot(Salary)

hist(YearsExperience)
hist(Salary)

#check for normality
qqnorm(YearsExperience)
qqline(YearsExperience)

qqnorm(Salary)
qqline(Salary)

#check correlation
cor(YearsExperience,Salary)

#perform linear regression
linreg4=lm(Salary~YearsExperience, data=salary_data)
summary(linreg4)

#predicted value
pred4 <- predict(linreg4,interval="predict")
pred4 <- as.data.frame(pred4)
View(pred4)

#R2 value is already 0.957 with original values so no transformation performed
