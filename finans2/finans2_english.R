library(scatterplot3d)

# Read the dataset 'finans2_data.csv' into R
D <- read.table("finans2/finans2_data.csv", header = TRUE, sep = ";")


# Subset containing only AGG, VAW, IWN and SPY (for validation)
D_test <- subset(D, ETF %in% c("AGG","VAW","IWN","SPY"))

# Subset containing only the 91 remaining ETFs (for model estimation)
D_model <- subset(D, !(ETF %in% c("AGG","VAW","IWN","SPY")))

scatterplot3d(D$Volatility, D$maxTuW, D$Geo.mean, zlab = "Geo.mean", xlab = "Volatility", ylab = "maxTuW")
plot(D$Volatility^2, D$Geo.mean, pch = 19, col = "black", xlab = "Volatility", ylab = "Geo.mean")
plot(D$Volatility, D$Geo.mean, pch = 19, col = "black", xlab = "Volatility", ylab = "Geo.mean")
plot(D$maxTuW, D$Geo.mean, pch = 19, col = "black", xlab = "maxTuW", ylab = "Geo.mean")
plot(D$Volatility, D$maxTuW, pch = 19, col = "black", xlab = "Volatility", ylab = "maxTuW")

hist(D$Geo.mean, xlab = "Geo.mean", main = "Histogram of Geo.mean", prob=TRUE)
hist(D$Volatility, xlab = "Volatility", main = "Histogram of Volatility", prob=TRUE)
hist(D$maxTuW, xlab = "maxTuW", main = "Histogram of maxTuW", prob=TRUE)


boxplot(D$Geo.mean, main="Geo.mean")
boxplot(D$Volatility, main="Volatility")
boxplot(D$maxTuW, main="maxTuW")

Geo.mean_mean = mean(D$Geo.mean)
Geo.mean_std = sd(D$Geo.mean)
Geo.mean_median = median(D$Geo.mean)
Geo.mean_quantiles = quantile(D$Geo.mean, probs=c(0.25, 0.75))
cat(Geo.mean_mean, Geo.mean_std, Geo.mean_median, Geo.mean_quantiles)

Volatility_mean = mean(D$Volatility)
Volatility_std = sd(D$Volatility)
Volatility_median = median(D$Volatility)
Volatility_quantiles = quantile(D$Volatility, probs=c(0.25, 0.75))
cat(Volatility_mean, Volatility_std, Volatility_median, Volatility_quantiles)

maxTuW_mean = mean(D$maxTuW)
maxTuW_std = sd(D$maxTuW)
maxTuW_median = median(D$maxTuW)
maxTuW_quantiles = quantile(D$maxTuW, probs=c(0.25, 0.75))
cat(maxTuW_mean, maxTuW_std, maxTuW_median, maxTuW_quantiles)

quantile(x <- rnorm(1001)) # Extremes & Quartiles by default
quantile(x,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)

# Estimate multiple linear regression model
v_q <- D_model$Volatility^3
v_sqr <- D_model$Volatility^2
v <- D_model$Volatility
y <- D_model$Geo.mean
m <- D_model$maxTuW^2

summary(lm(y ~ v_sqr))
summary(lm(y ~ v_sqr + v + m))

fit1 <- lm(y ~ v + m)
# Show parameter estimates etc.
summary(fit1)


# Plots for model validation

# Observations against fitted values
plot(fit1$fitted.values, D_model$Geo.mean, xlab = "Fitted values",     
       ylab = "Geom. average rate of return")

# Residuals against each of the explanatory variables
plot(D_model$Volatility, fit1$residuals, 
        xlab = "Volatility", ylab = "Residuals")
plot(D_model$maxTuW, fit1$residuals, 
     xlab = "maxTuW", ylab = "Residuals")

# Residuals against fitted values
plot(fit1$fitted.values, fit1$residuals, xlab = "Fitted values", 
     ylab = "Residuals")

# Normal QQ-plot of the residuals
qqnorm(fit1$residuals, ylab = "Residuals", xlab = "Z-scores", 
       main = "")
qqline(fit1$residuals)


# Confidence intervals for the model coefficients
confint(fit, level = 0.95)


# Predictions and 95% prediction intervals
pred <- predict(FINAL_MODEL, newdata = D_test, 
                interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(id = D_test$ETF, Geo.mean = D_test$Geo.mean, pred)

