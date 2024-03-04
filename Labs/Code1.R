
install.packages("ggplot2")
library(ggplot2)

install.packages("data.table")
library(data.table)

setwd("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/A-M-F-D-E-R/Test")
load("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/1/Prep/ceosal.RData")

dt.ceo.salaries <- data.table(data)

rm(data)

names(dt.ceo.salaries)

ncol(dt.ceo.salaries)

nrow(dt.ceo.salaries)

head(dt.ceo.salaries)

tail(dt.ceo.salaries)

View(dt.ceo.salaries)

dt.ceo.salaries[1, ] #first row and all cols

dt.ceo.salaries[ ,  salary] # all rows of var salary

dt.ceo.salaries[1, salary] # shows first row of var salary

dt.ceo.salaries[1:10, list(salary, age)] # first 10 rows of variables salary and age

dt.ceo.salaries[order(age)] #order ascending

dt.ceo.salaries[order(-age)] # order descending

dt.ceo.salaries[age<=45,] #select CEO less than 45

dt.young.ceo.salaries <- dt.ceo.salaries[age<=45,] # creates a new data table

dt.ceo.salaries[age<=45 & grad==1,] # Subseting the data using multiple conditions (use the symbol “&”" for and and the symbol “|” for or)

dt.ceo.salaries[, log_salary:=log(salary)] # Adding a new variable to the data.table:

dt.ceo.salaries[, age_squared:=age^2] 

dt.ceo.salaries[, log_salary:=NULL] # Deleting a variable from the data table


