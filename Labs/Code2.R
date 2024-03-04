
install.packages("data.table")
library(data.table)

install.packages("ggplot2")
library(ggplot2)

setwd("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/A-M-F-D-E-R/Test")




s <- rbinom(n=10, size=1, prob=0.5) 
coin.flips <- s
print(coin.flips)
sum(coin.flips)


X <- rbinom(n=1000, size=10, prob=0.5)
print(X)

ggplot(data = data.frame(X = factor(X)), aes(x = X)) + geom_histogram(stat = "count")



# Grades dataset

# load("")

dt.grades <- data.table(data)

rm(data)

dt.grades[1:10]

summary(dt.grades)

ggplot(data = dt.grades, aes(x = factor(final_grade))) + geom_histogram(stat = "count")

ggplot(data = dt.grades, aes(x = midterm, y = final_exam)) + geom_point()

ggplot(data = dt.grades, aes(x = midterm, y = final_exam)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

cov_final_midterm <- cov(dt.grades$final_exam, dt.grades$midterm) # Sample Covariance between midterm and final exam grades

mean_final_exam_subset <- mean(dt.grades[midterm < 10, "final_exam"]) # Sample Conditional Mean
mean_final_exam_subset <- mean(dt.grades[midterm >= 10, "final_exam"])


ggplot(dt.grades[, list(avg_final_exam = mean(final_exam)), by=list(midterm=round(midterm))]) + 
  geom_point(aes(x = midterm, y = avg_final_exam)) # Sample Conditional Mean with plot
