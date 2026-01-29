library(survival)

#Do patients who receive mido balanced with respect to other covariates
dat$insur <- as.factor(dat$insur)
dat$sex <- as.factor(dat$sex)
dat$reason <- as.factor(dat$reason)
dat$vaso <- as.factor(dat$vaso)

#subset the data
mido1 <- dat[dat$mido == 1, ]
mido0 <- dat[dat$mido == 0, ]

#how many patients stop mido?
(sum(dat$mido.stop)/204)*100
sum(dat$mido.stop)

#basic summary statistics mido vs no mido
summary(mido1)
summary(mido0)
sd(mido1$rr)
sd(mido0$rr)
sd(mido1$hr)
sd(mido0$hr)
sd(mido1$temp)
sd(mido0$temp)
sd(mido1$age)
sd(mido0$age)

#Determining normality of RR
hist(mido0$rr)
hist(mido1$rr)
plot(density(dat$rr))
qqnorm(dat$rr)
qqline(dat$rr)
shapiro.test(dat$rr)

#Determining normality of HR
hist(mido0$hr)
hist(mido1$hr)
plot(density(dat$hr))
qqnorm(dat$hr)
qqline(dat$hr)
shapiro.test(dat$hr)

#Determining normality of Temp
hist(mido0$temp)
hist(mido1$temp)
plot(density(dat$temp))
qqnorm(dat$temp)
qqline(dat$temp)
shapiro.test(dat$temp)

#Determining normality of Age
hist(mido0$age)
hist(mido1$age)
plot(density(dat$age))
qqnorm(dat$age)
qqline(dat$age)
shapiro.test(dat$age)

#continuous variables tests
wilcox.test(rr ~ mido, data = dat)
wilcox.test(hr ~ mido, data = dat)
wilcox.test(temp ~ mido, data = dat)
wilcox.test(age ~ mido, data = dat)

#Examining the differences in RR
summary(mido1$rr)
summary(mido0$rr)
plot(density(mido1$rr))
lines(density(mido0$rr), col=2)

#Examining the differences in HR
summary(mido1$hr)
summary(mido0$hr)
plot(density(mido1$hr))
lines(density(mido0$hr), col=2)

#Differences in binary catagorical variables

#Chi-square tests
chisq.test(table(dat$sex, dat$mido))
chisq.test(table(dat$dehyd, dat$mido))
chisq.test(table(dat$reason, dat$mido))
chisq.test(table(dat$vaso, dat$mido))
chisq.test(table(dat$shock, dat$mido))
chisq.test(table(dat$insur, dat$mido))

#2 way tables of interest
table(dat$sex, dat$mido)
table(dat$insur, dat$mido)
table(dat$dehyd, dat$mido)
table(dat$reason, dat$mido)
table(dat$vaso, dat$mido)
table(dat$shock, dat$mido)

#tables of proportions within mido no/yes groups
prop.table(table(dat$sex, dat$mido), margin = 2)
prop.table(table(dat$insur, dat$mido), margin = 2)
prop.table(table(dat$dehyd, dat$mido), margin = 2)
prop.table(table(dat$shock, dat$mido), margin = 2)
prop.table(table(dat$reason, dat$mido), margin = 2)
prop.table(table(dat$vaso, dat$mido), margin = 2)

#####################################################################
#Making a time dependent data set
#####################################################################

#To store each participant's data
temp.list <- vector("list", nrow(dat))
#loop for each participant
for (i in 1:nrow(dat)) {
  #initalize at the start of each new id
  start <- 0
  stop <- numeric(length=0)
  midodrine <- 0
  transfer <- numeric(length=0)
  #if mido was used
  if (dat$mido[i]==1) {
    start <- c(start, dat$time.mido[i])
    stop <- c(stop, dat$time.mido[i])
    midodrine <- c(midodrine,1)
    transfer <- c(transfer, 0)
  }
  #if mido was stopped
  if (dat$mido.stop[i]==1) {
    start <- c(start, dat$time.stop[i])
    stop <- c(stop, dat$time.stop[i])
    midodrine <- c(midodrine,0)
    transfer <- c(transfer, 0)
  }
  stop <- c(stop, dat$time.trans[i])
  transfer <- c(transfer, dat$trans[i])
  temp.list[[i]] <- data.frame(
    id = dat$id[i],
    start = start,
    stop = stop,
    transfer = transfer,
    midodrine = midodrine,
    rr = dat$rr[i],
    hr = dat$hr[i],
    temp = dat$temp[i],
    dehyd = dat$dehyd[i],
    vaso = dat$vaso[i],
    shock = dat$shock[i],
    reason = dat$reason[i],
    age = dat$age[i],
    sex = dat$sex[i],
    insur = dat$insur[i]
  )
}
mido.long <- do.call(rbind, temp.list)

#Data Checking
head(mido.long)
str(mido.long)

#Factor variables
mido.long$insur <- as.factor(mido.long$insur)
mido.long$sex <- as.factor(mido.long$sex)
mido.long$reason <- as.factor(mido.long$reason)
mido.long$vaso <- as.factor(mido.long$vaso)

#Fitting a time-dependent cox model
base.mod <- coxph(Surv(start,stop,transfer,type="counting")~midodrine, data=mido.long)
summary(base.mod)

# list of candidate variables
var.1 <- c("age", "rr", "hr", "temp", "vaso", "shock", "insur", "reason", "dehyd")
#data frame for results
results.1 <- data.frame(variable = var.1,
                        pvalue = NA)
#Loop to create and compare p-value of all midodrine + candidate variable models
for (i in seq_along(var.1)) {
  var <- var.1[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) +", var))
  fit <- coxph(f, data = mido.long)
  results.1$pvalue[i] <- summary(fit)$coefficients[2,"Pr(>|z|)"]
}
results.1
#Select HR, proceed to second loop
# list of candidate variables
var.2 <- c("age", "rr", "temp", "vaso", "shock", "insur", "reason", "dehyd")
#data frame for results
results.2 <- data.frame(variable = var.2,
                        pvalue = NA)
for (i in seq_along(var.2)) {
  var <- var.2[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) + hr +",
                         var))
  fit <- coxph(f, data = mido.long)
  results.2$pvalue[i] <- summary(fit)$coefficients[3,"Pr(>|z|)"]

}
results.2
#Select shock, proceed to third loop
# list of candidate variables
var.3 <- c("age", "rr", "temp", "vaso", "insur", "reason", "dehyd")
#data frame for results
results.3 <- data.frame(variable = var.3,
                        pvalue = NA)
for (i in seq_along(var.3)) {
  var <- var.3[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) + hr +
shock +", var))
  fit <- coxph(f, data = mido.long)
  results.3$pvalue[i] <- summary(fit)$coefficients[4,"Pr(>|z|)"]
}
results.3
#Select RR, proceed to fourth loop
# list of candidate variables
var.4 <- c("age", "temp", "vaso", "insur", "reason", "dehyd")
#data frame for results
results.4 <- data.frame(variable = var.4,
                        pvalue = NA)
for (i in seq_along(var.4)) {
  var <- var.4[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) +hr +
shock + rr +", var))
  fit <- coxph(f, data = mido.long)
  results.4$pvalue[i] <- summary(fit)$coefficients[5,"Pr(>|z|)"]
}
results.4
#Select dehyd, proceed to 5th loop
# list of candidate variables
var.5 <- c("age", "temp", "insur", "reason", "vaso")
#data frame for results
results.5 <- data.frame(variable = var.5,
                        pvalue = NA)
for (i in seq_along(var.5)) {
  var <- var.5[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) + hr +
shock + rr + dehyd +", var))
  fit <- coxph(f, data = mido.long)
  results.5$pvalue[i] <- summary(fit)$coefficients[6,"Pr(>|z|)"]
}
results.5
# list of candidate variables
var.6 <- c("age", "temp", "insur", "reason")
#data frame for results
results.6 <- data.frame(variable = var.6,
                        pvalue = NA)
for (i in seq_along(var.6)) {
  var <- var.6[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) + hr +
shock + rr + dehyd + vaso +", var))
  fit <- coxph(f, data = mido.long)
  results.6$pvalue[i] <- summary(fit)$coefficients[8,"Pr(>|z|)"]
}
results.6
fit <- coxph(Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) + hr + shock + rr +
               dehyd + vaso + reason, data = mido.long)
summary(fit)
#reason = trauma does have a significant p-value
var.7 <- c("age", "temp", "insur")
#data frame for results
results.7 <- data.frame(variable = var.7,
                        pvalue = NA)
for (i in seq_along(var.7)) {
  var <- var.7[i]
  f <- as.formula(paste0("Surv(start, stop, transfer, type='counting') ~ midodrine + strata(sex) + hr +
shock + rr + dehyd + vaso + reason +", var))
  fit <- coxph(f, data = mido.long)
  results.7$pvalue[i] <- summary(fit)$coefficients[10,"Pr(>|z|)"]
}
results.7
#no significant p-values, end loop

#No interactions model

mod.noin <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine+strata(sex)+rr+hr+shock+dehyd+vaso+
          dehyd+reason, data=mido.long)
summary(mod.noin)

mod.2 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine+strata(sex)+rr+hr+shock+vaso+dehyd,
        data=mido.long)
summary(mod.2)
anova(mod.noin, mod.2, test = "LRT")

#even with only one level being significant, reason should be included.

#Testing possible interactions with mido
mod.int1 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine*reason+strata(sex)+rr+hr+shock+vaso+
          dehyd, data=mido.long)
summary(mod.int1)
mod.int2 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine*rr+reason+strata(sex)+rr+hr+shock+va
        so+dehyd, data=mido.long)
summary(mod.int2)
mod.int3 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine*hr+reason+strata(sex)+rr+hr+shock+va
        so+dehyd, data=mido.long)
summary(mod.int3)
mod.int4 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine*shock+reason+strata(sex)+rr+hr+shock
        +vaso+dehyd, data=mido.long)
summary(mod.int4)
mod.int5 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine*vaso+reason+strata(sex)+rr+hr+shock+
          vaso+dehyd, data=mido.long)
summary(mod.int5)
mod.int6 <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine*dehyd+reason+strata(sex)+rr+hr+shock
        +vaso, data=mido.long)
summary(mod.int6)
#Conclude no meaningful interactions


#MODEL DIAGNOSITICS AND DATA SUMMARY
#Check proportional Hazards
cox.zph(mod.noin)
#assumption of proportional hazards not violated
#Quantiles for HR
hrq <- quantile(mido.long$hr, probs=c(.25,.5,.75))
mido.long$hrc <- cut(mido.long$hr, c(-Inf,hrq,Inf))
mido.long$hrc
mido.long$hrc <- as.numeric(mido.long$hrc)
mido.mods <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine+strata(sex)+rr+strata(hrc)+shock+vaso
        +reason, data=mido.long)
summary(mido.mods)
summary(mod.noin)

#Check proportional Hazards
cox.zph(mido.mods)
par(mfrow = c(1, 1)) # Resets layout to single plot
par(mar = c(5, 4, 4, 2) + 0.1) # Resets margins to default


#final model
mod.noin <-
  coxph(Surv(start,stop,transfer,type="counting")~midodrine+strata(sex)+rr+hr+shock+dehyd+vaso+
          reason, data=mido.long)
summary(mod.noin)

#plotting survival curves
#mean or most common values
mean_rr <- mean(dat$rr)
mean_hr <- mean(dat$hr)
mode_shock <- names(which.max(table(dat$shock)))
mode_dehyd <- names(which.max(table(dat$dehyd)))
mode_vaso <- names(which.max(table(dat$vaso)))
mode_reason <- names(which.max(table(dat$reason)))
mean(dat$rr)
mean(dat$hr)
names(which.max(table(dat$shock)))
names(which.max(table(dat$dehyd)))
names(which.max(table(dat$vaso)))
names(which.max(table(dat$reason)))
#create data frame
newdat <- data.frame(
  midodrine = c(0, 1),
  sex = c("Female", "Female", "Male","Male"),
  rr = mean_rr,
  hr = mean_hr,
  shock = 0,
  dehyd = 0,
  vaso = "norepinephrine",
  reason = "other"
)
newdat
#plot typical curves
s <- survfit(mod.noin, newdata = newdat)
summary(s)
plot(s, col= c(1,2,1,2), lty = c(1,1,2,2), xlab = "Time (minutes)", ylab = "Probability of Not Being
Transferred",
     main = "Adjusted Survival Curves, Midodrine Use Stratified by Sex")
legend(90, 0.9, legend = c("Female: No Midodrine", "Female: Midodrine","Male: No Midodrine",
                           "Male: Midodrine"),col= c(1,2,1,2), lty = c(1,1,2,2))

#Unadjusted baseline curve
plot(survfit(Surv(start, stop, transfer) ~ midodrine, data = mido.long), col=c(1:2), xlab = "Time
(minutes)", ylab = "Probability of Not Being Transferred",
     main = "Unadjusted Survival Curves for Midodrine Use")
legend(90, 0.9, legend = c("No Midodrine", "Midodrine"),col= c(1,2), lty = 1)


#Plot of deviance residuals
dev <- residuals(mod.noin, type = "deviance")

plot(dev, main="Deviance residuals")

#CS residuals
mido.r <- residuals(mod.noin, type="martingale")
mido.cs <- mido.long$transfer-mido.r
mido.cs
cs.fit <- survfit(Surv(mido.cs,transfer)~1,data=mido.long)
plot(cs.fit, fun="cumhaz", xlab="Residual", ylab="H(residual)", main="Midodrine Data - Cox-Snell
Residual Plot", conf.int=F)
abline(c(0,1), col="red")
