D <- read.table("project/finans1_data.csv", header=TRUE, sep=";", as.is=TRUE)
D <- D[ ,c("t","AGG","VAW","IWN","SPY")]

## Dimensions of D (number of rows and columns)
dim(D)
## Column/variable names
names(D)
## The first rows/observations
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset
str(D)

AGG_mean <- mean(D$AGG)
AGG_sd <- sd(D$AGG)

VAW_mean <- mean(D$VAW)
VAW_sd <- sd(D$VAW)

IWN_mean <- mean(D$IWN)
IWN_sd <- sd(D$IWN)

SPY_mean <- mean(D$SPY)
SPY_sd <- sd(D$SPY)

AGG_seq <- seq(min(D$AGG), max(D$AGG), 0.001)
VAW_seq <- seq(min(D$VAW), max(D$VAW), 0.001)
IWN_seq <- seq(min(D$IWN), max(D$IWN), 0.001)
SPY_seq <- seq(min(D$SPY), max(D$SPY), 0.001)

## Histogram describing the empirical density of the weekly returns from
## AGG (histogram of weekly returns normalized to have an area of 1)
hist(D$AGG, xlab="Return (AGG)", prob=TRUE)
lines(AGG_seq, dnorm(AGG_seq, AGG_mean, AGG_sd), lwd=2)

hist(D$VAW, xlab="Return (VAW)", prob=TRUE, ylim=c(0, 13))
lines(VAW_seq, dnorm(VAW_seq, VAW_mean, VAW_sd), lwd=2)

hist(D$IWN, xlab="Return (IWN)", prob=TRUE, ylim=c(0,13))
lines(IWN_seq, dnorm(IWN_seq, IWN_mean, IWN_sd), lwd=2)

hist(D$SPY, xlab="Return (SPY)", prob=TRUE)
lines(SPY_seq, dnorm(SPY_seq, SPY_mean, SPY_sd), lwd=2)


## Converts the variable 't' to a date variable in R
D$t <- as.Date(x=D$t, format="%Y-%m-%d")
## Checks the result
summary(D$t)

## Plots of weekly return over time for each of the four ETFs
ylim <- c(-0.2,0.2)
## Plot of weekly return over time for AGG
plot(D$t, D$AGG, type="l", ylim=ylim, xlab="Date", ylab="Return AGG")
  ## Similar plots for the three other ETFs
plot(D$t, D$VAW, type="l", ylim=ylim, xlab="Date", ylab="Return VAW")
plot(D$t, D$IWN, type="l", ylim=ylim, xlab="Date", ylab="Return IWN")
plot(D$t, D$SPY, type="l", ylim=ylim, xlab="Date", ylab="Return SPY")

## Box plot of weekly returns by ETF
boxplot(D$AGG, D$VAW, D$IWN, D$SPY, names=c("AGG", "VAW", "IWN", "SPY"),
        xlab="ETF", ylab="Return")

sum(!is.na(D$AGG))
sum(!is.na(D$VAW))
sum(!is.na(D$IWN))
sum(!is.na(D$SPY))

AGG_mean <- mean(D$AGG, na.rm=TRUE)
VAW_mean <- mean(D$VAW, na.rm=TRUE)
IWN_mean <- mean(D$IWN, na.rm=TRUE)
SPY_mean <- mean(D$SPY, na.rm=TRUE)
cat(AGG_mean, VAW_mean, IWN_mean, SPY_mean)


AGG_var <- var(D$AGG, na.rm=TRUE)
VAW_var <- var(D$VAW, na.rm=TRUE)
IWN_var <- var(D$IWN, na.rm=TRUE)
SPY_var <- var(D$SPY, na.rm=TRUE)
cat(signif(AGG_var,4), signif(VAW_var,4), signif(IWN_var,4), signif(SPY_var,4))

AGG_sd <- sd(D$AGG, na.rm=TRUE)
VAW_sd <- sd(D$VAW, na.rm=TRUE)
IWN_sd <- sd(D$IWN, na.rm=TRUE)
SPY_sd <- sd(D$SPY, na.rm=TRUE)
cat(signif(AGG_sd,4), signif(VAW_sd,4), signif(IWN_sd,4), signif(SPY_sd,4))

q <- c(0.25, 0.5, 0.75)
AGG_quantiles <- quantile(D$AGG, probs=q)
VAW_quantiles <- quantile(D$VAW, probs=q)
IWN_quantiles <- quantile(D$IWN, probs=q)
SPY_quantiles <- quantile(D$SPY, probs=q)
cat(AGG_quantiles, "\n", VAW_quantiles, "\n", IWN_quantiles, "\n", SPY_quantiles)


qqnorm(D$AGG, main="Normal Q-Q Plot of AGG")
qqline(D$AGG)

qqnorm(D$VAW, main="Normal Q-Q Plot of VAW")
qqline(D$VAW)

qqnorm(D$IWN, main="Normal Q-Q Plot of IWN")
qqline(D$IWN)

qqnorm(D$SPY, main="Normal Q-Q Plot of SPY")
qqline(D$SPY)

AGG_mean - qt(0.975, df=453)*AGG_sd/sqrt(454)

AGG_mean - qnorm(0.975)*AGG_sd/sqrt(454)

tobs_AGG <- AGG_mean/AGG_sd*sqrt(454)
tobs_VAW <- AGG_mean/VAW_sd*sqrt(454)
tobs_IWN <- AGG_mean/IWN_sd*sqrt(454)
tobs_SPY <- AGG_mean/SPY_sd*sqrt(454)

t.test(D$AGG, conf.level=0.95)
t.test(D$VAW, conf.level=0.95)
t.test(D$IWN, conf.level=0.95)
t.test(D$SPY, conf.level=0.95)

p_val_AGG <- 2*(1-pt(abs(tobs_AGG), df=453))
p_val_VAW <- 2*(1-pt(abs(tobs_VAW), df=453))
p_val_IWN <- 2*(1-pt(abs(tobs_IWN), df=453))
p_val_SPY <- 2*(1-pt(abs(tobs_SPY), df=453))

t.test(D$AGG, mu=0)

tobs_mu = (AGG_mean - VAW_mean)/sqrt(AGG_sd^2/454 + VAW_sd^2/454)
v = (AGG_sd^2/454 + VAW_sd^2/454)^2/((AGG_sd^2/454)^2/453 + (VAW_sd^2/454)^2/453)
pval_mu = 2*(1-pt(abs(tobs_mu),df=v))
pval_mu

t.test(D$VAW, D$AGG)

corr< - 0

#centering data around zero

SPY_centered <- D$SPY - SPY_mean
AGG_centered <- D$AGG - AGG_mean

VAW_centered_overSD <- (D$VAW - VAW_mean)/VAW_sd
IWN_centered_overSD <- (D$IWN - IWN_mean)/IWN_sd

IWN_VAW_corr <- sum(IWN_centered_overSD * VAW_centered_overSD)/453
IWN_VAW_corr

plot(D$VAW, D$IWN, xlab="VAW", ylab="IWN", main="VAW against IWN correlation")
plot(VAW_centered_overSD, IWN_centered_overSD, xlab="centered VAW data over sample std", ylab="centered IWN data over sample std",
     main="Centered VAW against IWN correlation")

cor(D[ ,c("VAW","IWN")], use="pairwise.complete.obs")
