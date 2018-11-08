rm(list=ls())
gc()

setup
attach(antisemitism_07)

# define factors
str(antisemitism_07)
liedetection<- as.factor(liedetection)
suffering<-as.factor(suffering)

#label factors
liedetection<- factor(liedetection,c(0,1),labels = c("Without","With"))
suffering<-factor(suffering,c(1,2),labels = c("ongoing","limited"))
gender<-factor(gender,c(1,2),labels = c("male","female"))

# 1. descriptive statistics
#gender ratio
table(gender)
#install.packages("psych")
library(psych)
describe(antisemitism_07,fast=TRUE)                                                                        

# 2. sample size
f<-function(r_sq){sqrt(r_sq/(1-r_sq))}
f(.12)
#We calculated the sample size in G power afterwards

# 3. Visualizing data
#3.1 Box plot with two factor variables
boxplot(prejudice ~ suffering*liedetection, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), 
        ylab="prejudice",
        xlab = "suffering x liedetection")
#3.2 Two-way interaction plot
interaction.plot(x.factor = suffering, trace.factor = liedetection, 
                 response = prejudice, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "liedetection", ylab="prejudice",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   

# 4. Anova
#4.1 Balanced?
table(liedetection, suffering)  

#4.2 Design is unbalanced, so type II is the way to go!
#install.packages("car")
mod1<-lm(prejudice ~ suffering * liedetection)
Anova(mod1,type = "II")
summary(mod1)


#ANOVA results: statistically insignificant main effects and the interaction effect.

#4.3 effect size
#install.packages("sjstats")
library(sjstats)
fit <- Anova(aov(prejudice ~ suffering * liedetection), type='II')
omega_sq(fit)
##The anova_stats() function takes a model input and computes a comprehensive summary,
#including the above effect size measures, 
#returned as tidy data frame (as tibble, to be exact).

# 5. chack model assumptions
#5.1 Homoscedasticity
plot(mod1, 1)
#Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
library(car)
leveneTest(prejudice ~ suffering * liedetection)
#the p-value is not less than the significance level of 0.05. 
#variance across groups is not significantly different. 
#we can assume the homogeneity of variances in the different treatment groups.

#5.2 Normality  
#As all the points fall approximately along this reference line, 
#we can assume normality.
plot(mod1, 2)

#Extract the residuals
aov_residuals<-residuals(object = mod1)
#Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# 6. post-hoc test
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(mod1,
                 pairwise~suffering * liedetection,
                 adjust="tukey")
posthoc
plot(posthoc)



