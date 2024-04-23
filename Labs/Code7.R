set.seed(1984)
n1 <- 25000
x1 <- rnorm(n = n1, mean = 0, sd = 3) # Independent variable 1
x2 <- rnorm(n = n1, mean = 0, sd = 4) # Independent variable 2
e <- rnorm(n = n1, mean = 0, sd = 2) # Error term
y <- 2 + 3*x1 + 4*x2 + e # Dependent variable
sorting <- rnorm(n = n1, mean = 0, sd = 2) # Variable for sorting and sampling
# Creating and organizing the data table
dt.population <- data.table(y, x1, x2, sorting)
dt.population <- dt.population[order(sorting)] # Randomizing data

ssize <- 2000 # Sample size
r.sample.rows <- sample(1:nrow(dt.population), size = ssize) 
r.sample <- dt.population[r.sample.rows, ]
ols1 <- lm(y ~ x1 + x2, data = r.sample)
summary(ols1)

bootreps <- 5 # Number of bootstrap repetitions for demonstration 
boot.resultsvec <- numeric(bootreps)

for (j in 1:bootreps) {
  r.sample.rows <- sample(1:nrow(dt.population), size = ssize) 
  r.sample <- dt.population[r.sample.rows, ]
  olsboot <- lm(y ~ x1 + x2, data = r.sample) 
  boot.resultsvec[j] <- coef(olsboot)["x1"]
}
boot.resultsvec


bootreps <- 10000
boot.resultsvec <- numeric(bootreps)

for (j in 1:bootreps) {
  r.sample.rows <- sample(1:nrow(dt.population), size = ssize) 
  r.sample <- dt.population[r.sample.rows, ]
  olsboot <- lm(y ~ x1 + x2, data = r.sample) 
  boot.resultsvec[j] <- coef(olsboot)["x1"]
}

# Sorting results and extracting confidence intervals
boot.resultsvec <- sort(boot.resultsvec)
conf.low <- boot.resultsvec[round(bootreps*0.025)]
conf.up <- boot.resultsvec[round(bootreps*0.975)]

# Display the 95% confidence interval
list(conf.low = conf.low, conf.up = conf.up)


# Fit the linear regression model
ols <- lm(y ~ x1 + x2, data = dt.population)

# Compute the asymptotic confidence interval using normal theory
asymptotic_CI <- confint(ols)["x1", ]

# Display the asymptotic confidence interval
asymptotic_CI
