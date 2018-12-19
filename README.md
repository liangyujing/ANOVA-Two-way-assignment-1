# Clear workspace
rm(list=ls())

# Load the ExperienceSampling_Group7.csv
attach(ExperienceSampling_Group7)
my_data<-ExperienceSampling_Group7
summary(my_data)

# label factors
str(my_data)
before_exam<-factor(beepnum<24)
after_exam<-factor(beepnum>23)
PA<-factor(labels = c("PA")

# 1. Visualizing data
## Scatter Plot
scatter.smooth(x=beepnum, y=PA, main="PA ~ beepnum")

# 2. Plot the data
Dummy <- ifelse(beepnum<24,0,1)

fit1 <- lm(formula = my_data$PA~my_data$beepnum*Dummy)
fit2 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,2,raw=TRUE))
fit3 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,3,raw=TRUE))
fit4 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,4,raw=TRUE))
fit5 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,5,raw=TRUE))
fit6 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,6,raw=TRUE))
fit10<- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,10,raw=TRUE))

summary(fit1); summary(fit2); summary(fit3);summary(fit4);
summary(fit5);summary(fit6);summary(fit10);
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit10)

#splines
library(splines)
plot(PA, main = "Beepnum versus PA", xlab="beepnum",pch=20)
lines(stats::lowess(PA), col='black')
lines(predict(fit1), col='red')
lines(predict(fit2), col='green')
lines(predict(fit3), col='orange')
lines(predict(fit10), col='steelblue')
legend(x = "bottomright", legend = c("Lowess","Linear fit", 
                                      "Quandratic fit","Cubic fit",
                                      "Overfitted(Degree 10)"), 
                                    lwd = rep(3,4),
                                   col = c("black","red","green","orange","steelblue"),
                                   text.width = 17,cex = 0.80)

# 3. R squares and adjusted R squares
#using data "mtcars" as the example
list.1 <- c(seq(1,8),15)  # The iteration numbers

#Retrieve
r.squared.list.1 <- lapply(list.1, function(x) summary(lm(my_data$PA~poly(my_data$beepnum*Dummy,x,raw=T)))$r.squared)
adj.r.squared.list.1 <- lapply(list.1, function(x) summary(lm(my_data$PA~poly(my_data$beepnum*Dummy,x,raw=T)))$adj.r.squared)

#ggplot2
library(ggplot2)
my.data.plot <- data.frame(list.1,unlist(r.squared.list.1),unlist(adj.r.squared.list.1))
colnames(my.data.plot) <- c("Degrees","RSquared","AdjRSquared")
#
ggplot(my.data.plot, aes(Degrees)) + 
  geom_line(aes(y = RSquared, colour = "RSquared")) + 
  geom_point(aes(y = RSquared,colour = "RSquared"), shape = 17) +
  geom_line(aes(y = AdjRSquared, colour = "AdjRSquared")) + 
  geom_point(aes(y = AdjRSquared,colour = "AdjRSquared"), shape = 5)+ 
  xlab("Degrees") +
  ylab("Combined R Values") +
  ggtitle("AdjR^2 and R^2") 

       
# 4. Cross validation
##perform K-fold cross-validation with K=10 
library(boot)
set.seed(1)
deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy, i), data = my_data)
  deltas[i] <- cv.glm(my_data, fit, K = 10)$delta[1]
}
lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,2,raw=TRUE))
plot(1:10, deltas, xlab = "Degree of polynomial", ylab = "CV MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)
##D=8 is the optimal degree for the polynomial.


# 5. Information criteria
#calculating AIC and BIC values of each model
AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit10)
BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit10)  
#fit5 has lowest AIC and BIC. 
#fit3 is slightly higher than fit5.
#both AIC and BIC has a preference for lm5. 

# 6. predict PA at beepnum = 91
lm3 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,3,raw=TRUE))
y <- my_data$PA
a <- my_data$beepnum
lm3 <- lm(y~a+poly(a,3,raw=TRUE))
predict(lm3,newdata = data.frame(a=91))
predict(lm3, newdata = data.frame(a=91),interval = "confidence")
##the 95% confidence interval associated with a beepnum of 91 is (60.92 85.42). 

# diagnostic plots
par(mfrow=c(2,2))
plot(lm3)
##First one: residuals versus predicted values is useful for checking the assumption of linearity and homoscedasticity.
##Second one: the normality assumption
##Third plot: scale-location plot, the assumption of homoscedasticity
##The fourth plot helps us find influential cases


# Evaluate Nonlinearity
# component + residual plot aka partial-residual plots
library("car")
crPlots(fit2,data=my_data)



##You should see a green line that models the residuals of your predictor against your dependent variable (i.e., the loess line). The red line represents the line of best fit. If your green line seems to be similarly linear as your red line, you’re good. If the green line appears curved relative to the red line, you likely have a linearity problem.

##Homoscedasticity Assumption
ncvTest(fit2)
##p < .05, suggesting that our data is not homoscedastic. 

# Outliers
outlierTest(fit2) # Bonferonni p-value for most extreme obs
##23 outliers

