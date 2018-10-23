library(survival) # loading survival functions into R
library(KMsurv) 

data(bmt)

fit <- survfit(Surv(t1, d1) ~ 1, data = bmt)

summary(fit)
