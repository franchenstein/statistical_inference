library(ggplot2)

#Seed setting. This way, every simulation will have the same results:
set.seed(4)
lambda <- 0.2
numSimulations <- 1000
sampSize <- 40

#Matrix where each row is a realization of the exponential distribution with parameter lambda
#and sampSize samples:
simulation <- matrix(rexp(numSimulations*sampSize, rate = lambda), numSimulations, sampSize)
#Calculating the mean for each simulation:
simMeans <- rowMeans(simulation)

#Histogram showing the distribution of sample means:
hist(simMeans,
     breaks = 20,
     prob = TRUE,
     main = "Average of Samples Distribution, 
     drawn from exponential distribution, 
     lambda = 0.2",
     xlab = "")
#Uniting the lines of the histogram for better visualization of the distribution:
lines(density(simMeans))
#Horizontal line indicating where the theoretical mean is (1/lambda):
abline(v = 1/lambda, col = "blue")

#Generating the expected normal distribution with mean 1/lambda:
x <- seq(min(simMeans), max(simMeans), length = numSimulations)
y <- dnorm(x, mean = 1/lambda, sd = 1/lambda/sqrt(sampSize))
lines(x, y, pch = 22, col = "blue", lty = 2)
legend('topright',c("simulation", "theoretical"), lty = c(1,2), col = c("black", "blue"))

#Using qqnorm to compare how close the simulated distribution is to the normal:
qqnorm(simMeans)
qqline(simMeans)

#Evaluating confidence interval:
#Lambda values interval that will be considered:
lambdas <- seq(4, 6, by = 0.01)
coverage <- NULL
#For each lambda in the interval:
for (i in 1:length(lambdas)){
    #numSimulations of sampSize of the original distribution:
     sim <- matrix(rexp(sampSize*numSimulations, rate= lambda), numSimulations, sampSize)
     mu <- rowMeans(sim)
     aux <- qnorm(0.975)*sqrt(1/lambda**2/sampSize)
     lower <- mu - aux
     upper <- mu + aux
     m <- mean(lower < lambdas[i] & upper > lambdas[i])
     coverage <- rbind(coverage, m)
}
qplot(lambdas, coverage) + geom_hline(yintercept = 0.95)