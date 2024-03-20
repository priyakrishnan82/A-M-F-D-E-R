
# Install and load necessary library

install.packages("MASS")
install.packages("stargazer")

library(stargazer)
library(MASS)

#Homework
#Part A

# Set seed for reproducibility
set.seed(420711)

# Generate data and perform regression analysis

#4a
# Set sample size
ssize <- 1000

# Generate x1 and x2 variables
x1 <- rnorm(ssize, sd = 3)
x2 <- rnorm(ssize, sd = 5)

# Generate response variable y
y <- 2 + 3 * x1 + 5 * x2 + rnorm(ssize, sd = 5)

# Fit regression models with both x1 and x2, only x1, and only x2
out.y.full <- lm(y ~ x1 + x2)
out.y.x1.om <- lm(y ~ x1)
out.y.x2.om <- lm(y ~ x2)

# Perform correlation test between x1 and x2
correlation <- cor.test(x1, x2)

# Print regression results
stargazer(out.y.full, out.y.x1.om, out.y.x2.om,
          type = 'text', omit.stat = c('f', 'ser'), no.space = TRUE)

# Generate new x1 and x2 with x2 dependent on x1
x1 <- rnorm(ssize, sd = 3)
x2 <- rnorm(ssize, mean = x1 / 2, sd = 5)

# Generate new response variable y
y <- 2 + 3 * x1 + 5 * x2 + rnorm(ssize, sd = 5)

# Fit regression models with both x1 and x2, only x1, and only x2
out.y.full <- lm(y ~ x1 + x2)
out.y.incomp.x1 <- lm(y ~ x1)
out.y.incomp.x2 <- lm(y ~ x2)
out.x1.parti <- lm(x1 ~ x2)
out.x2.parti <- lm(x2 ~ x1)

# Print regression results for each model separately
stargazer(out.x1.parti, type = 'text', omit.stat = c('f', 'ser'))
stargazer(out.x2.parti, type = 'text', omit.stat = c('f', 'ser'))
stargazer(out.y.full, type = 'text', omit.stat = c('f', 'ser'))
stargazer(out.y.incomp.x1, type = 'text', omit.stat = c('f', 'ser'))
stargazer(out.y.incomp.x2, type = 'text', omit.stat = c('f', 'ser'))

# Quantify bias for x1 and x2
bias_x1 <- coef(out.y.full)['x1'] * coef(out.x1.parti)['x2']  
bias_x2 <- coef(out.y.full)['x2'] * coef(out.x2.parti)['x1']

# Compare coefficients from partial model to real coefficients plus bias
coef_x1_comparison <- c(
  coef(out.y.full)['x1'] + coef(out.y.full)['x2'] * coef(out.x2.parti)['x1'],
  coef(out.y.incomp.x1)['x1']
)
coef_x2_comparison <- c(
  coef(out.y.full)['x2'] + coef(out.y.full)['x1'] * coef(out.x1.parti)['x2'],
  coef(out.y.incomp.x2)['x2']
)

# Print results
print("Bias for x1:")
print(bias_x1)
print("Bias for x2:")
print(bias_x2)
print("Coefficient comparison for x1:")
print(coef_x1_comparison)
print("Coefficient comparison for x2:")
print(coef_x2_comparison)

#4b

ssize <- 1000
x1 <- rnorm(n=ssize, sd=3)
x2 <- rnorm(n=ssize,mean=x1*1.33, sd=5)
y <- 2+3*x1+5*x2+rnorm(n=ssize,sd=5)
out.y.full <- lm(y ~x1 + x2)
out.parti.x2 <- lm(x1 ~x2)
out.y.x1 <- lm(y ~residuals(out.parti.x2))
out.y.x1.om <- lm(y ~ x1)
stargazer(
  out.y.full,
  out.x2.parti,
  out.parti.x2,
  out.y.x1.om,
  out.y.x1,
  type='text',
  omit.stat=c('f','ser'), no.space=T)

out.y.full
predicted_y_full <- predict(out.y.full) 
bias_full <- mean(y - predicted_y_full)
bias_full

#4c

ssize <- 1000
x1 <- rnorm(n = ssize, sd = 3)
x2 <- rnorm(n = ssize, mean = x1 * 1.33, sd = 5)
y <- 2 + 3 * x1 + 5 * x2 + rnorm(n = ssize, sd = 5)
out.y.full <- lm(y ~ x2)
out.y.full
summary(out.y.full) 
conf_intervals <- confint(out.y.full)
conf_intervals 



#Part B
print("Part B")

#1
# Set seed for reproducibility
set.seed(420711)

# Generate sample data
library(MASS)
sigma <- matrix(c(1,0.2,0.1,0.35,0,0.2,1,0,0.4,0,0.1,0,1,0,0.4,0.35,0.4,0,1,0.6,0,0,0.4,0.6,1), 5, 5)
vect <- mvrnorm(5347, rep(0, 5), sigma)

# Extract variables
Y=vect[,1]
X1=vect[,2]
X2=vect[,3]
Z1=vect[,4]
Z2=vect[,5]


# Covariance matrix
cov_matrix <- cov(vect)
print(cov_matrix)


#2
# Coefficient of Y on X1
xbar=mean(X1)
ybar=mean(Y)
beta1=sum((X1-xbar)*(Y-ybar))/sum((X1-xbar)^2)
beta1

beta1 <- lm(Y ~ X1)$coef[2]
print(beta1)

#3
# Get X
X=matrix(cbind(1,X1,X2,Z1,Z2),5347,5)
X

# Get X'X
XtX <- t(X) %*% X
print(XtX)

# Inverse of X'X
inv_XtX <- solve(XtX)
print(inv_XtX)

# Check if XtX * inv_XtX equals identity matrix
print(XtX %*% inv_XtX)

# Calculate beta_hat
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_hat)

# Coefficients from lm() function
lm_coef <-lm(Y~X1+X2+Z1+Z2)$coef
print(lm_coef)

#5
# Define number of bins and compute bin edges
num_bins <- 10
bin_edges <- quantile(X1, probs = seq(0, 1, length.out = num_bins + 1))

# Compute bin indices for each observation
bin_indices <- cut(X1, breaks = bin_edges, labels = FALSE)

# Compute the mean of Y within each bin
bin_means <- tapply(Y, bin_indices, mean)

# Plot CEF
plot(bin_edges[-1], bin_means, type = "l", col = "blue", 
     xlab = "X1", ylab = "Mean of Y",
     main = "Conditional Expectation Function (CEF) for Y|X1")
