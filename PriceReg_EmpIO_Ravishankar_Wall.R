# Third part: price regression
# Here you focus only on the mean price AND the mean yield (price divided by direct distance). 
# You can add as an explanatory variable a dummy for whether there is the possibility to connect 
# via a hub (which could be endogenous). I suggest that instead of using the control function 
# approach of Gayle and Wu,you use City2 as an instrument for N∗, the number of active ﬁrms. 
# If you have another idea for an instrument for N∗, I am open to suggestions.

#------------------------------ SML Procedure -------------------------------#
rm(list=ls())
if (Sys.info()["user"] == "alecr") {
  projdir <- "C:/Users/alecr/OneDrive/Documents/TSE_2023-24/Empirical IO/Project/"
}

library(AER)

price_data <- readRDS(paste0(projdir, "price_data.rds"))

# OLS regression
price.ols <- lm(log(mprice) ~ pop + income + dist + dist2 + slot + nentrants + nentrythreats,
                data = price_data)
summary(price.ols)

# Parameter estimates are a mixed bag compared to those from paper pretty well already:
# pop: 0.25 here vs. 0.54
# income: 0.51 here vs. -0.06 ...
# dist: -0.45 here vs. 5..
# dist2: 0.001 here vs. -5.66...
# slot: 0.12 here vs. 0.09
# nentrants: -0.15 here vs. -0.05
# nentrythreats: -0.034 vs. -0.008

# IV regression
price.iv <- ivreg(log(myield) ~ pop + income + dist + dist2 + slot + hub + nentrants + nentrythreats | 
                    pop + income + dist + dist2 + slot + hub + nentrythreats + City2,
                  data = price_data)
summary(price.iv)

# Parameter estimates seem to match those from paper pretty well already:
# pop: 0.69 here vs. 0.54
# income: 0.44 here vs. -0.06 ...
# dist: 1.44 here vs. 5..
# dist2: 0.0001 here vs. -5.66...
# slot: 0.31 here vs. 0.09
# nentrants: -0.28 here vs. -0.05
# nentrythreats: 0.075 vs. -0.008...

