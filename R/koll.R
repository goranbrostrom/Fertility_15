load("fert.rda")
library(eha)
fert$age <- cut(fert$enter, c(15, 20, 25, 30, 35, 40, 45, 50))
fert$age <- cut(fert$enter, c(14, 20, 25, 30, 35, 40, 45, 50))

fit <- coxreg(Surv(exit - enter, event > 0.5) ~ strata(age) + civst +
                 birthdate, data = fert[fert$parity == 0, ])
fit1 <- coxph(Surv(exit - enter, event > 0.5) ~ strata(age) + civst +
                 birthdate, data = fert[fert$parity == 0, ])
summary(fit1)
