
setwd("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/8/Labs_Panel_DiD-20240416/")

require(data.table) # you can use library or require 
require(ggplot2) # you can use library or require require(stargazer) # you can use library or require 

load("fastfood3.RData")
load("fastfood4.RData")
load("fastfood.RData")

head(dt.fastfood)

plot1 <- ggplot( data = dt.fastfood, aes(x = wage))
plot1 + geom_density() + facet_wrap( ~ state + time) + theme_bw()

qplot(
  data = dt.fastfood, x = factor(time), y = emptot
  , fill = factor(state)
  , geom = 'boxplot') + theme_bw() + xlab("time") + ylab("FTE Employment")

dt.bf.aft <- data.table(dt.fastfood) # Create a new table called dt.bf.aft
dt.bf.aft <- dt.bf.aft[, list(
  mean_emptot = mean(emptot, na.rm = TRUE), # Calculate mean of emptot
  mean_wage = mean(wage, na.rm = TRUE),     # Calculate mean of wage
  mean_pmeal = mean(pmeal, na.rm = TRUE),   # Calculate mean of pmeal
  mean_hrsopen = mean(hrsopen, na.rm = TRUE) # Calculate mean of hrsopen
), by = list(state, time)] # Specify the list of grouping variables


dt.bf.aft.clean <- dt.fastfood[!is.na(wage),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(pmeal),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(emptot),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(hrsopen),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(emptot),]
dt.bf.aft.clean <- data.table(dt.fastfood.clean)
dt.bf.aft.clean <- dt.bf.aft.clean[, list(
  mean_emptot  = mean(emptot  , na.rm=TRUE)
  , mean_wage    = mean(wage    , na.rm=TRUE)
  , mean_pmeal   = mean(pmeal   , na.rm=TRUE)
  , mean_hrsopen = mean(hrsopen , na.rm=TRUE)
), by=list(state, time)]
dt.bf.aft.clean

t.test(  dt.fastfood.clean[state==0 & time==0, emptot]
         , dt.fastfood.clean[state==1 & time==0, emptot])

t.test(  dt.fastfood.clean[state==0 & time==1, emptot]
         , dt.fastfood.clean[state==1 & time==1, emptot])

lm1 <- lm( emptot ~ time + state + time*state, data = dt.fastfood.clean)
stargazer(lm1, type = "text")

coeffs <- coefficients(lm1)
coeffs

dt.bf.aft.clean



