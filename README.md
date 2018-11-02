# data-assignment-1
attach(antisemitism_07)

#1. sample size
# install.packages("pwr")
require(pwr)
f<-function(r_sq){sqrt(r_sq/(1-r_sq))}
pwr.anova.test(k=4,f=f(.12),power=.90,sig.level=.05)#解释选择那个η的原因

#2. Anova

#2.1 Two-way interaction plot
interaction.plot(x.factor = liedetection, trace.factor = suffering, 
                 response = prejudice, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "liedetection", ylab="prejudice",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   


#2.2 Balanced?
table(liedetection, suffering)  

#2.3 Design is unbalanced, so type III is the way to go!
#install.packages("car")
my_anova1 <- aov(prejudice ~ suffering * liedetection)
Anova(my_anova1,type = "III") 
summary(my_anova1)

my_anova2 <- aov(prejudice ~ suffering + liedetection)
Anova(my_anova2,type = "III") 
summary(my_anova2)



# 2.4 pairwise comparisons
ls_lie = lsmeans(modII, pairwise~lie, adjust="tukey")
ls_gender = lsmeans(modII, pairwise~gender, adjust="tukey")
ls_race = lsmeans(modII, pairwise~race, adjust="tukey")

(prejudice ~ suffering * liedetection

# Homogeneity of variances
plot(res.aov3, 1)

#Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
library(car)
leveneTest(prejudice ~ suffering * liedetection)

#the p-value is not less than the significance level of 0.05. 
#variance across groups is not  significantly different. 
#we can assume the homogeneity of variances in the different treatment groups.


# 2. Normality  
#As all the points fall approximately along this reference line, we can assume normality.
plot(res.aov3, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )











