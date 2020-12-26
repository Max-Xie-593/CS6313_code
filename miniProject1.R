# QUESTION 1
# Part a

# coding in the probability density function
q1_funct <- function(x) {
  if(x[1] < 0) { # return 0 if value is negative
    return(c(0))
  }
  # otherwise it is the pdf
    result <- 0.2 * exp(-0.1 * x) - 0.2 * exp(-0.2 * x)
    return(c(result))

}

# The lifetime exceeds 15 years means P{X > 15}
# Therefore we do an integration from 15 to infinity
# integrate returns more data than just value, we need value
ans1a <- integrate(q1_funct,15,Inf)$value

# Part b
# part i

# using rexp to generate randon variables
blockA <- rexp(1,0.1) #variables to replicate the blocks
blockB <- rexp(1,0.1)
ans1bi <- max(blockA,blockB)

# part ii
# repeat for 10000 times
holdDraws <- replicate(10000,max(rexp(1,0.1),rexp(1,0.1)))

# part iii
# creating a histogram
hist(holdDraws, probability = TRUE)
# impose the density function
curve(expr = sapply(x, q1_funct),add = TRUE)

# part iv
# E(T) is the mean
average <- mean(holdDraws)

# part v
# return a list of draws that are greater than 15
gt15 <- holdDraws[holdDraws > 15]
# divide over total list to get a probability
prob <- length(gt15)/length(holdDraws)

# part vi
# 1000 pulls (code to perform the same steps from b)
holdDraws1k <- replicate(1000,max(rexp(1,0.1),rexp(1,0.1)))
average1k <- mean(holdDraws1k)
average1k
gt15_1k <- holdDraws1k[holdDraws1k > 15]
prob1k <- length(gt15_1k)/length(holdDraws1k)
prob1k

# 100000 pulls (code to perform the same steps from b)
holdDraws100k <- replicate(100000,max(rexp(1,0.1),rexp(1,0.1)))
average100k <- mean(holdDraws100k)
average100k
gt15_100k <- holdDraws100k[holdDraws100k > 15]
prob100k <- length(gt15_100k)/length(holdDraws100k)
prob100k

# END OF QUESTION 1

# QUESTION 2
# Define a function to test a random point is within a unit circle
estPi <- function() {return((runif(1)^2 + runif(1)^2) <= 1)}

# repeat for 10000 rolls
listEstPi <- replicate(10000,estPi())
# grab values inside circle
numInPI <- listEstPi[listEstPi == TRUE]
# Estimate PI
piEst <- 4 * (length(numInPI) / length(listEstPi))

# for reference, hold the value of error from the real pi
valerror <- abs(piEst - pi)
