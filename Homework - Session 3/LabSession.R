install.packages("data.table")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("plm")
install.packages("lmtest")


library(data.table)
library(ggplot2)
library(stargazer) 
library(plm)
library(lmtest)
library(stats)

setwd("/Users/priyakrishnan/Library/CloudStorage/Dropbox/PhD/Others/Applied-Methods-for-Digital-Economy-Research/7/Labs_Panel_DiD-20240409")

load("rental.RData")
head(dt.rental)

out.ols <- plm(  lrent ~  pctstu + lpop + lavginc + y90
                 , c = index("city", "year")
                 , model = "pooling"
                 , data=dt.rental)
stargazer(out.ols, type = "text")



out.ols <- plm(  rent ~  pctstu + pop + avginc + y90
                 , c = index("city", "year")
                 , model = "pooling"
                 , data=dt.rental)
stargazer(out.ols, type = "text")

u <- residuals(out.ols)
length(u)

dt.rental[,'u'] <- u
head(dt.rental)

dt.rental$L1_u <- c(NA, dt.rental$u[-nrow(dt.rental)])
summary(dt.rental$L1_u)

out.u <- plm( u ~ L1_u, data=dt.rental, model='pooling')
stargazer(out.u, type="text")

pdt.rental   <- pdata.frame( dt.rental,  index = c("city", "year"))

out.fd <- plm(  lrent ~  pctstu + lpop + lavginc,
                , model = "fd"
                , data=pdt.rental)
stargazer(out.fd, type = "text")

bptest(out.fd)

stargazer(coeftest(out.fd, vcov. = vcovHC(out.fd, method = c("arellano"))), type = "text")

out.fe <- plm(  lrent ~  0 + pctstu + lpop + lavginc + y90
                , c = index("city", "year")
                , model = "within"
                , data=dt.rental)
stargazer(out.fe, type = "text")


