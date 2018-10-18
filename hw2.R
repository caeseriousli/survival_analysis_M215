t1 = c(1, 22, 3, 12, 8, 17, 2, 11, 8, 12, 2, 5, 4, 15, 8, 23, 5, 11, 4, 1, 8)
c1 = rep(1, 21)

t2 = c(10, 7, 32, 23, 22, 6, 16, 34, 32, 25, 11, 20, 19, 6, 17, 35, 6, 13, 9, 6, 10)
c2 = c(1, 1, 0, 1, 1, 1, 1, 0, 0, 0,0,0,0,1,0,0,1,1,0,0,0)

lam = 0.025
x = seq(0, 150, by=0.01)
y = lam*exp(-lam*x)

library(ggplot2)
library(survival) # loading survival functions into R
library(KMsurv)

surv = Surv(t1, c1)
fit <- survfit(surv ~ 1, data = surv)

plot(-log(fit$surv) ~ fit$time, type = 's', col = 'blue',
     main = 'Estimated Cumulative Hazard Function', ylab = 'H(t) = -log[S(t)]',
     xlab = 'Time (t)')

d = data.frame(fit$surv, fit$time)
ggplot(data = d, aes(x=fit.time, y=fit.surv)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  stat_function(fun = pexp, n = 101, args = list(rate=lam, lower.tail=FALSE)) +
  ggtitle("Emperical Survival Function vs. Parametric Estimation") +
  xlab("Time") + ylab("Probability")


######## 3.6 ##########
t1 = c(5,8,12,24,32,17,16,17,19,30)
c1 = c(1,1,1,1,1,1,0,0,0,0)