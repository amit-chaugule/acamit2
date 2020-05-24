

#---------------Q1----------
cutlet_data=read.csv("E:/Excelr/Assignments/03_Hyporthesis_Testing/Cutlets.csv")
View(cutlet_data)
attach(cutlet_data)
#check normality of both the columns/population
shapiro.test(Unit.A)
shapiro.test(Unit.B)
#check variance. p_value > 0.05 (significance)
var.test(Unit.A,Unit.B)
#As A and B are 2 Discrete variables and output variable diameter is a continuous, we will go with 2-sample T test
#Ho: Avg of diameter of Unit A is equal to Avg of diameter of Unit B
#Ha: Avg of diameter of Unit A is NOT equal to Avg of diameter of Unit B
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct=TRUE)
#t.test(Unit.A,Unit.B,conf.level = 0.95,var.equal = T)
#t.test(Unit.A,Unit.B,conf.level = 0.95,paired=T)
#since pvalue > 0.05; p hgh null Fly
#There is no significant difference in diameter of Unit A and Unit B

boxplot(Unit.A,Unit.B)

#---------------Q2----------
lab_data=read.csv("E:/Excelr/Assignments/03_Hyporthesis_Testing/LabTAT.csv")
View(lab_data)
attach(lab_data)
#check normality of both the columns/population
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)
#check variance. p_value > 0.05 (significance)
var.test(Laboratory.1,Laboratory.2)
# Ho: Variance of Laboratory.1 is equal Laboratory.3
# Ha: Variance of Laboratory.1 is NOT equal Laboratory.3
#pvalue(0.01366)<0.05
var.test(Laboratory.1,Laboratory.3)
var.test(Laboratory.1,Laboratory.4)
var.test(Laboratory.2,Laboratory.3)
var.test(Laboratory.2,Laboratory.4)
var.test(Laboratory.3,Laboratory.4)
#As A and B are 2 Discrete variables and output variable diameter is a continuous, we will go with 2-sample T test
#Ho: Avg TAT of all Labs are equal
#Ha: Avg TAT of all Labs are NOT equal
# MANOVA test
res.man <- manova(cbind(Laboratory.1, Laboratory.2,Laboratory.4) ~ Laboratory.3, data = lab_data)
summary(res.man)
summary.aov(res.man)

--annova--
# stacked <- stack(lab_data)
# stacked
# annova <- aov(values~ind,data=stacked)
# summary(annova)
# boxplot(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4)
#since pvalue < 0.05; p low null go
#There is significant difference in Avg TAT among different laboratory

#---------------Q3----------
buyer_data=read.csv("E:/Excelr/Assignments/03_Hyporthesis_Testing/BuyerRatio.csv")
View(buyer_data)
attach(buyer_data)
tab <- data.frame(buyer_data[,2:5])
tab
chisq.test(tab)
#Ho: All proportions are equal
#Ha: All proportions are NOT equal
#since p_value > 0.05; phigh null fly; We accept null hypothesis that the ratios are equal across all regions.

#---------------Q4----------
customer_data=read.csv("E:/Excelr/Assignments/03_Hyporthesis_Testing/CostomerOrderForm.csv")
View(customer_data)
attach(customer_data)
data <- data.frame(customer_data)
View(data)
stacked <- stack(data)
stacked
tab <- table(stacked)
tab
plot(tab)
chisq.test(tab)
# Ho: Defective % equal across centre
# Ha: Defective % vary by centre
#p_value(0.2771) > 0.05; p high null fly;
#defect % does not vary by centre.
barplot(tab)

#---------------Q5----------
shop_data=read.csv("E:/Excelr/Assignments/03_Hyporthesis_Testing/Fantaloons.csv")
View(shop_data)
attach(shop_data)
tab <- table(shop_data)
tab
prop.test(tab)
#Ho: % of males versus females is equal
#Ha: % of males versus females is NOT equal 
#p_value>0.05; p high null fly; accept null hypothesis
barplot(tab)

