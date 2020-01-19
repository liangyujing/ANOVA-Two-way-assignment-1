rm(list=ls())

# Set your working dir as the current dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data csv file
library("readr")
my_data <- read_csv("R code/antisemitism_07.csv")
summary(my_data)

# define factors
str(antisemitism_07)
liedetection<-as.factor(liedetection)
suffering<-as.factor(suffering)

# label factors
liedetection<-factor(liedetection,c(0,1),labels = c("Absent","Present"))
suffering<-factor(suffering,c(1,2),labels = c("Ongoing","Limited"))
gender<-factor(gender,c(1,2),labels = c("male","female"))

# 1. descriptive statistics
# gender ratio
table(gender)
# mean, sd
aggregate(antisemitism_07, list(liedetection,suffering), mean)
aggregate(antisemitism_07, list(liedetection,suffering), sd)

# 2. sample size
f<-function(r_sq){sqrt(r_sq/(1-r_sq))}
f(.12)
#We calculated the sample size in G power afterwards

# 3. Visualizing data
#3.1 Box plot with two factor variables
boxplot(prejudice ~ suffering*liedetection,frame = FALSE, col = c("#00AFBB", "#E7B800"),
        ylab="prejudice",
        xlab = "suffering x liedetection")
#boxplot
library("dplyr")
groups <- group_by(my_data_gender_T, valence, membership)
plot.data <- summarise(groups,
                       mean = mean(value, na.rm=TRUE),
                       sd = sd(value, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)
ggplot(plot.data, aes(x=valence, y=mean, fill = membership)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,7)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=position_dodge(.9)) +
  ggtitle("Mean rating by valence and membership")
  

#3.2 Two-way interaction plot
interaction.plot(x.factor = suffering, trace.factor = liedetection, 
                 response = prejudice, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "suffering", ylab="prejudice",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   




# 4. Anova
#4.1 Balanced?
table(liedetection, suffering)  

#4.2 Design is unbalanced, so type II is the way to go!
#install.packages("car")
library(car)
mod1<-lm(prejudice ~ suffering * liedetection)
Anova(mod1,type = "II")
summary(mod1)

#ANOVA results: statistically insignificant main effects and interaction effect.

#4.3 effect size
#install.packages("sjstats")
library(sjstats)
library(car)
fit <- Anova(aov(prejudice ~ suffering * liedetection), type='II')
omega_sq(fit)

# 5. check model assumptions
#5.1 Homoscedasticity
plot(mod1, 1)
#Use the Levene’s test to check the homogeneity of variances. The function leveneTest() [in car package] will be used:
library(car)
leveneTest(prejudice ~ suffering * liedetection)
#the p-value is not less than the significance level of 0.05. 
#variance across groups is not significantly different. 
#we can assume the homogeneity of variances in the different treatment groups.

#5.2 Normality  
plot(mod1, 2)
#As all the points fall approximately along this reference line, 
#we can assume normality.

#Extract the residuals
aov_residuals<-residuals(object = mod1)
#Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# 6. post-hoc test   ## 多重比较
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(mod1,
                 pairwise~suffering * liedetection,
                 adjust="tukey")
posthoc
plot(posthoc)


# 7. outlier  ##极端值
#boxplot
boxplot(prejudice)
boxplot(prejudice, outline = FALSE)

#Anova_with outlier
library(car)
mod1<-lm(prejudice ~ suffering * liedetection)
Anova(mod1,type = "II")
summary(mod1)

#Anova_without outlier  ## 剔除极端值
library(dplyr)
result <- filter(antisemitism_07,
                 prejudice>-2.445497902 & prejudice<2.555471349)
mod2<-lm(data=result,prejudice ~ suffering * liedetection)
Anova(mod2,type = "II")
summary(mod2)
