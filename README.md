rm(list=ls())
gc()

attach(antisemitism_07)

# defeine factors
str(antisemitism_07)
liedetection<- as.factor(liedetection)
suffering<-as.factor(suffering)
head(antisemitism_07)

# 1. sample size
#install.packages("pwr")
require(pwr)
f<-function(r_sq){sqrt(r_sq/(1-r_sq))}
pwr.anova.test(k=4,f=f(.12),power=.90,sig.level=.05) #解释选择那个η的原因
pwr.2way(k=4,f=f(.12),power=.90,sig.level=.05)
#pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, f.A=0.8, f.B=0.4)


# 2. Anova
#2.1 Visualizing data
#label factors
liedetection<- factor(liedetection,c(0,1),labels = c("Without","With"))
suffering<-factor(suffering,c(1,2),labels = c("ongoing","limited"))

# 总的来说
#install.packages("HH") 
fit <- aov(prejudice ~ suffering * liedetection)
library(HH)
interaction2wt(prejudice ~ suffering * liedetection)


# 2.1.1 Box plot with two factor variables
boxplot(prejudice ~ suffering*liedetection, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), 
        ylab="prejudice",
        xlab = "suffering x liedetection")


# 2.1.2 Two-way interaction plot
interaction.plot(x.factor = liedetection, trace.factor = suffering, 
                 response = prejudice, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "liedetection", ylab="prejudice",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   


# 2.2 Balanced?
table(liedetection, suffering)  

# 2.3 Design is unbalanced, so type III is the way to go!
#install.packages("car")
mod1<-lm(prejudice ~ suffering * liedetection)
anova_stats(car::Anova(mod1, type = 3))
summary(mod1)

#old:Anova(mod1,type = "III") 

#ANOVA results: statistically insignificant main effects and the interaction effect.

#2.4 effect size
#install.packages("sjstats")
library(sjstats)
fit <- mod1
omega_sq(fit)


##The anova_stats() function takes a model input and computes a comprehensive summary,
#including the above effect size measures, 
#returned as tidy data frame (as tibble, to be exact):

# 2.5 chack model assumptions
# 2.5.1 Homogeneity of variances
plot(mod1, 1)

#Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
library(car)
leveneTest(prejudice ~ suffering * liedetection)

#the p-value is not less than the significance level of 0.05. 
#variance across groups is not significantly different. 
#we can assume the homogeneity of variances in the different treatment groups.


# 2.5.2 Normality  
#As all the points fall approximately along this reference line, we can assume normality.
plot(mod1, 2)

# Extract the residuals
aov_residuals <- residuals(object = mod1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


# 2.6 post-hoc test
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(mod1,
                 pairwise~suffering * liedetection,
                 adjust="tukey")
posthoc
plot(posthoc)



