# Lab03_simpleOLS

install.packages("ggplot2")
library(ggplot2)

install.packages("data.table")
library(data.table)

install.packages(stargazer)
library(stargazer)

install.packages(hmisc)
library(hmisc)

sales <- read.csv("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/1/Core Materials-20240205/sales-data.csv")

dt.sales <- data.frame(sales)

rm(sales)

# Explore the data

ncol(dt.sales)
nrow(dt.sales)
colnames(dt.sales)
stargazer(dt.sales, type = "text")
summary(dt.sales)
head(dt.sales)

# Plots - need to correct

qplot( data = dt.sales
       , x = advertising
       , y = sales
       , geom = "point") +
  theme_bw()

# Correlation

dt.sales[, cor(sales, advertising)]

#dt.sales[, rcorr(sales, advertising)]

# regression

m.sales <- lm(sales ~ advertising, data=dt.sales)


summary(lm.sales)

stargazer(lm.sales, type = "text")

