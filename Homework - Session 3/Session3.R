

install.packages("MASS")

library(MASS)



#Part B
print("Part B")
# Set seed for reproducibility
#1
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

# Coefficient of Y on X1
#2
xbar=mean(X1)
ybar=mean(Y)
beta1=sum((X1-xbar)*(Y-ybar))/sum((X1-xbar)^2)
beta1

beta1 <- lm(Y ~ X1)$coef[2]
print(beta1)

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
