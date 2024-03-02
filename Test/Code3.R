install.packages("ggplot2")

getwd()

list.files()

library(ggplot2)

X <- rbinom(n=1000, size=10, prob=0.75) 
ggplot(data = data.frame(X = factor(X)), aes(x = X)) + geom_bar(stat = "count", color = "red") 

data

BOD

