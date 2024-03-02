# Lab02_Descriptives

install.packages("ggplot2")
library(ggplot2)

install.packages("data.table")
library(data.table)

install.packages(stargazer)
library(stargazer)

setwd("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/A-M-F-D-E-R/Test")

load("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/1/Prep/ceosal.RData")

dt.ceosal <-data.table(data)

rm(data)

nrow(dt.ceosal)

# How many CEOS have a graduate degree?
dt.ceosal[, sum(grad)]

nrow(dt.ceosal[grad==1,])

# Percentage of CEOs with graduate degrees
dt.ceosal[, mean(grad)]

dt.ceosal[, sum(grad)]/nrow(dt.ceosal)

# Mean of salary

dt.ceosal[, mean(salary)]

mean(dt.ceosal[, salary])

# Mean CEO salary with graduate degree

dt.ceosal[grad==1, mean(salary)]

# Mean CEO salary without graduate degree

dt.ceosal[grad==0, mean(salary)]

# List how many CEOS with and without college degree

dt.ceosal[, list(n_ceo=.N), by=college]

# Is the mean statistically different from 800

t.test(dt.ceosal[, salary], mu=800)

# is there a sig diff between CEO with and without graduate degree

t.test(dt.ceosal[, salary] ~ dt.ceosal[, grad])

t.test(dt.ceosal[grad==1, salary], dt.ceosal[grad==0, salary])

# Descriptive statistics for salary

dt.ceosal[, list( mean_salary = mean(salary)
                , sd_salary = sd(salary)
                , min_salary =  min(salary)
                , max_salary = max(salary)
                , median_salary = median(salary))]


# Descriptive statistics for salary by grad and college

dt.ceosal[, list( mean_salary = mean(salary)
                  , sd_salary = sd(salary)
                  , min_salary =  min(salary)
                  , max_salary = max(salary)
                  , median_salary = median(salary)) , by = list(grad, college)]

# summary stats of all variables

stargazer(dt.ceosal, type = "text")

# summary stats of age, sal based on whether grad

stargazer(dt.ceosal[grad==1, list(salary, age)], type = "text")



# Histogram - need to correct

ggplot( data = dt.ceosal,
        aes(x = salary) +
        geom.histogram(stat = "count"))


# Do geom = line, point, bar for above option



# Facet Wrap - need to correct

qplot(   data = dt.ceo.salaries
         , x = salary
         , geom = "histogram") + facet_wrap(~ grad)





# Cutomising plots - need to corect

qplot(   data = dt.ceo.salaries
         , x = salary
         , geom = "histogram"
         ,  fill = factor(grad, levels = c(0,1), labels = c("Yes", "No"))) +
  theme_bw() +
  ylim(0,50) +
  xlim(0, 4000) +
  labs( title = "MY PLOT", x = "CEO Salary", y = "Number of CEOs", fill = "Grad. Degree")





