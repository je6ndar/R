par(mfrow=c(2,3), mar=c(3,3,1,1)+0.1)
par(cex=0.8)

plotit <- function(n, p){
    plot(0:n, dbinom(0:n, n, p), type="h", xlab="", ylab="",
         main=paste0("$np = ",n*p,"$, $n(1-p) = ",n*(1-p),"$"))
}

plotit(n=6, p=0.5)
plotit(n=9, p=1/3)
plotit(n=18, p=1/3)
plotit(n=30, p=1/3)
plotit(n=45, p=1/3)
plotit(n=100, p=0.3)



# Testing the probability = 0.5 with a two-sided alternative
# We have observed 518 out of 1154
# Do it without continuity corrections

prop.test(x=518, n=1154, p = 0.5, correct = FALSE)


# Testing that the probabilities for the two groups are equal
# Calculating 99% confindece interval
prop.test(x=c(23,35), n=c(57,167), correct=FALSE, conf.level=0.99)


# Reading the data into R
pill.study <- matrix(c(23, 35, 34, 132), ncol = 2, byrow = TRUE)
rownames(pill.study) <- c("Blood Clot", "No Clot")
colnames(pill.study) <- c("Pill", "No pill")
pill.study

# Chi^2 test for tesing that the distribution for the two groups are equal
chisq.test(pill.study, correct = FALSE)

# If we want the expected numbers, then store the result in a variable
chi <- chisq.test(pill.study, correct = FALSE)

# In the result the expected values can be found
chi$expected


# Reading the data into R
poll <- matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), ncol = 3, 
               byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

# Column percentages
colpercent <- prop.table(poll, 2)
colpercent

barplot(t(colpercent), beside = TRUE, col = 2:4, las = 1, 
        ylab = "Percent each week", xlab = "Candidate", 
        main = "Distribution of Votes")
legend( legend = colnames(poll), fill = 2:4,"topright", cex = 0.7)


# Testing same distribution in the three populations
chi <- chisq.test(poll, correct = FALSE)
chi

# Expected values
chi$expected


# Reading the data into R
results <- matrix(c(23, 60, 29, 28, 79, 60, 9, 49, 63), ncol = 3, 
                  byrow = TRUE)
colnames(results) <- c("MathBad", "MathAve", "MathGood")
rownames(results) <- c("EngBad", "EngAve", "EngGood")


# Percentages
prop.table(results)

# Row totals
margin.table(results, 1)

# Column totals
margin.table(results, 2)


# Testing independence between english and maths results
chi <- chisq.test(results, correct = FALSE)
chi

# Expected values
chi$expected
