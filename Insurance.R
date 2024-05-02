#Load the data into R
summary(insurance)

#Split your data into training and test sets
insurance_sample <- sample(c(TRUE, FALSE), nrow(insurance), replace = T, prob = c(0.6,0.4))
# subset data points into train and test sets
insurance_train <- insurance[insurance_sample, ]
insurance_test <- insurance[!insurance_sample, ]


# Create your linear regression models
insurance_model <- lm(charges ~ bmi + age + sex + children + smoker, data = insurance_train)


# Obtain residuals from the fitted model
residuals <- residuals(insurance_model)


# Test for autocorrelation using Durbin-Watson test
# Install the 'lmtest' package if you haven't already
install.packages("lmtest")
library(lmtest)


# Perform Durbin-Watson test
dwtest(insurance_model)

# Plot autocorrelation function (ACF) of residuals
# Install the 'stats' package if you haven't already
acf(residuals)




hist(insurance$charges)



plot(charges ~ bmi, data=insurance)

plot(charges ~ age, data=insurance)

plot(charges ~ sex, data=insurance)

plot(charges ~ children, data=insurance)

plot(charges ~ smoker, data=insurance)


summary(insurance_model)


#Check for homoscedasticity
par(mfrow=c(2,2))
plot(insurance_model)
par(mfrow=c(1,1))







# Load ggplot2 library
install.packages("ggplot2")
library(ggplot2)



#save predicted and residual values to df
insurance_train$estimate <- predict(insurance_model)
insurance_train$residuals <- residuals(insurance_model)

#create visualization
ggplot(insurance_train, aes(bmi, charges)) +
  geom_point() + #plot actual values
  geom_point(aes(y = estimate), color = "blue") +
  geom_segment(aes(xend = bmi, yend = estimate), color = "gray")



# Add a LOESS smoother to your scatterplot

ggplot(insurance_train, aes(bmi, charges )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red") 



#create visualization
ggplot(insurance_train, aes(age, charges)) +
  geom_point() + #plot actual values
  geom_point(aes(y = estimate), color = "blue") +
  geom_segment(aes(xend = age, yend = estimate), color = "gray")



# Add a LOESS smoother to your scatterplot

ggplot(insurance_train, aes(age, charges )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red") 



#create visualization
ggplot(insurance_train, aes(sex, charges)) +
  geom_point() + #plot actual values
  geom_point(aes(y = estimate), color = "blue") +
  geom_segment(aes(xend = sex, yend = estimate), color = "gray")



# Add a LOESS smoother to your scatterplot

ggplot(insurance_train, aes(sex, charges )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red") 



#create visualization
ggplot(insurance_train, aes(smoker, charges)) +
  geom_point() + #plot actual values
  geom_point(aes(y = estimate), color = "blue") +
  geom_segment(aes(xend = smoker, yend = estimate), color = "gray")



# Add a LOESS smoother to your scatterplot

ggplot(insurance_train, aes(smoker, charges )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red") 





#create visualization
ggplot(insurance_train, aes(children, charges)) +
  geom_point() + #plot actual values
  geom_point(aes(y = estimate), color = "blue") +
  geom_segment(aes(xend = children, yend = estimate), color = "gray")



# Add a LOESS smoother to your scatterplot

ggplot(insurance_train, aes(children, charges )) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red") 

