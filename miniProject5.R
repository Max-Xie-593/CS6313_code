# QUESTION 1

# read in the csv
peopleData <- read.csv("bodytemp-heartrate.csv",header = TRUE, sep = ",")
# get male and female data
maleData = peopleData[1:65,]
femaleData = peopleData[66:130,]

# part a
# grab body temperature data
x = maleData$body_temperature
y = femaleData$body_temperature

# exploratory analysis of the body temperature data
boxplot(x,y, main='male vs female body temperature', ylab = "temperature", names = c("male","female"))
qqplot(x,y, main='male vs female body temperature')
abline(0,1)
hist(x,col = rgb(0,0,1,alpha = 0.5), main = 'Histogram of body temperature', xlab = 'temperature')
hist(y,col = rgb(1,0,0,alpha = 0.5), add=T)
box()

# t test
t.test(x,y)

# part b
# grab body temperature data
x = maleData$heart_rate
y = femaleData$heart_rate

# exploratory analysis of the body temperature data
boxplot(x,y, main='male vs female heart rate', ylab = "heart rate", names = c("male","female"))
qqplot(x,y, main='male vs female heart rate')
abline(0,1)
hist(x,col = rgb(0,0,1,alpha = 0.5), main = 'Histogram of heart rate', xlab = 'heart rate')
hist(y,col = rgb(1,0,0,alpha = 0.5), add=T)
box()

# t test
t.test(x,y)

# part c
# exploratory analysis
plot(peopleData$body_temperature,peopleData$heart_rate,main = 'body temperature vs heart rate')
abline(lm(peopleData$heart_rate~peopleData$body_temperature))
plot(maleData$body_temperature,maleData$heart_rate,main = 'body temperature vs heart rate for males')
abline(lm(maleData$heart_rate~maleData$body_temperature))
plot(femaleData$body_temperature,femaleData$heart_rate,main = 'body temperature vs heart rate for females')
abline(lm(femaleData$heart_rate~femaleData$body_temperature))

# corrlation test(s)
cor.test(peopleData$body_temperature,peopleData$heart_rate)
cor.test(maleData$body_temperature,maleData$heart_rate)
cor.test(femaleData$body_temperature,femaleData$heart_rate)

# END OF QUESTION 1


# QUESTION 2

# data important for answering this question
lambdas = c(0.01, 0.1, 1, 10)
n = c(5, 10, 30, 100)

# part a
# defining functions for computing Monte Carlo estimate of coverage probabilities of the 2 interval
inside_z_interval <- function(sample,lambda,alpha=0.05) {
  double_side_z_score <- qnorm(1 - (alpha / 2))
  size <- length(sample)
  sampleMean <- mean(sample)
  standard_error <- sd(sample) / sqrt(size)
  range <- double_side_z_score * standard_error
  
  z_interval <- (sampleMean + c(-1,1) * range)
  mu = 1 / lambda
  return(mu >= z_interval[1] && mu <= z_interval[2])
}
z_score_coverage_percent <- function(size,lambda) {
  coverageTests <- replicate(5000,inside_z_interval(rexp(n = size,rate = lambda),lambda))
  return(sum(coverageTests == 1) / length(coverageTests))
}

inside_parametric_bootstrap_interval <- function(size, lambda, alpha = 0.05) {
  mu = 1 / lambda
  nboot <- 1000
  
  x <- rexp(size, lambda)
  sample_x_lambda <- 1 / mean(x)
  boots <- replicate(nboot, mean(rexp(size,sample_x_lambda)))
  
  confidence_interval = sort(boots)[c(ceiling(nboot*alpha/2),ceiling(nboot*(1-alpha/2)))]
  mu = 1 / lambda
  return(mu >= confidence_interval[1] && mu <= confidence_interval[2])
}

parametric_bootstrap_coverage_percent <- function(size,lambda) {
  coverageTests <- replicate(5000,inside_parametric_bootstrap_interval(size,lambda))
  return(sum(coverageTests == 1) / length(coverageTests))
}

# part b
# large-sample z-interval computation
z_results <- rep(0, length(n) * length(lambdas))
dim(z_results) <- c(length(n), length(lambdas))
rownames(z_results) <- c("n = 5","n = 10","n = 30","n = 100")
colnames(z_results) <- c("lambda = 0.01","lambda = 0.1","lambda = 1","lambda = 10")

for(x in 1:length(n)) {
  for (y in 1:length(lambdas)) {
    z_results[x,y] = z_score_coverage_percent(n[x],lambdas[y])
  }
}

# parametric bootstrap interval computation
boot_results <- rep(0, length(n) * length(lambdas))
dim(boot_results) <- c(length(n), length(lambdas))
rownames(boot_results) <- c("n = 5","n = 10","n = 30","n = 100")
colnames(boot_results) <- c("lambda = 0.01","lambda = 0.1","lambda = 1","lambda = 10")

for(x in 1:length(n)) {
  for (y in 1:length(lambdas)) {
    boot_results[x,y] = parametric_bootstrap_coverage_percent(n[x],lambdas[y])
  }
}

# part c
# summary of the Monte Carlo Results from part b

# Z interval percent coverage vs n
colors <- rainbow(length(z_results[1,]))
plot(x = n, y = z_results[,1], type = "o", main = "z interval percent coverage vs n", ylab = "percent coverage", xlab = "n", col = colors[1], ylim = c(min(z_results),max(z_results)))
for (i in 2: length(z_results[1,])) {
  lines(x = n, y = z_results[,i], col = colors[i], type = "o", pch = 20 + i)
}
legend("topleft",title = "Fixed Lambda", legend = c("0.01", "0.01", "1", "10"), col = colors, pch = 21:24,lty = 1)

# Z interval percent coverage vs lambda
colors <- rainbow(length(z_results[,1]))
plot(x = lambdas, y = z_results[1,], type = "o", main = "z interval percent coverage vs lambda", ylab = "percent coverage", xlab = "lambda", col = colors[1], ylim = c(min(z_results),1))
for (i in 2: length(z_results[,1])) {
  lines(x = lambdas, y = z_results[i,], col = colors[i], type = "o", pch = 20 + i)
}
legend("topleft",title = "Fixed N", legend = c("5", "10", "30", "100"), col = colors, pch = 21:24,lty = 1)

# parametric bootstrap interval percent coverage vs n
colors <- rainbow(length(boot_results[1,]))
plot(x = n, y = boot_results[,1], type = "o", main = "parametric bootstrap interval percent coverage vs n", ylab = "percent coverage", xlab = "n", col = colors[1], ylim = c(min(boot_results),max(boot_results)))
for (i in 2: length(boot_results[1,])) {
  lines(x = n, y = boot_results[,i], col = colors[i], type = "o", pch = 20 + i)
}
legend("topleft",title = "Fixed Lambda", legend = c("0.01", "0.01", "1", "10"), col = colors, pch = 21:24,lty = 1)

# parametric bootstrap interval percent coverage vs lambda
colors <- rainbow(length(boot_results[,1]))
plot(x = lambdas, y = boot_results[1,], type = "o", main = "parametric bootstrap interval percent coverage vs lambda", ylab = "percent coverage", xlab = "lambda", col = colors[1], ylim = c(min(boot_results),0.97))
for (i in 2: length(boot_results[,1])) {
  lines(x = lambdas, y = boot_results[i,], col = colors[i], type = "o", pch = 20 + i)
}
legend("topleft",title = "Fixed N", legend = c("5", "10", "30", "100"), col = colors, pch = 21:24,lty = 1)


# part d

# END OF QUESTION 2