# QUESTION 1
# read data
cancerdata <- read.csv(file = 'prostate_cancer.csv')

# PSA scatterplot
plot(cancerdata$psa,ylab = "psa",main = "PSA")
plot(log(cancerdata$psa),ylab = "Log(psa)",main = "Log(psa)")

# boxplot of weights with (possible) outliers
boxplot(x = cancerdata$weight, main = "Weight")

# remove outliers
cancer <- cancerdata[-which(cancerdata$weight %in% (boxplot(cancerdata$weight,plot = FALSE)$out))]
y <- log(cancerdata$psa)

# plot again without outliers
plot(x = cancer$weight, y = y, main = "Log(psa) us weight w/o outliers")
firstlogfit <- lm(formula = (y ~ weight), data = cancer)
abline(firstlogfit)
summary(firstlogfit)

# plot cancervol. I am not sure of the distribution, so as suggested by the assignment, let us transform it
plot(cancer$cancervol)
cancer$cancervol <- log(cancer$cancervol)
plot(cancer$cancervol)

# plotting the cancervol
plot(cancer$cancervol,y = y, main = "Log(psa) vs Log(cancervol)")
secondlogfit <- lm(formula = y ~ cancer$cancervol, data = cancer)
abline(secondlogfit)
summary(secondlogfit)

# plotting the age
plot(x = cancer$age, y = y, main = "Log(psa) vs age")
thirdfit <- lm(formula = (y ~ cancer$age), data = cancer)
abline(thirdfit)
summary(thirdfit)

# plotting the benpros
plot(x = cancer$benpros, y = y, main = "Log(psa) vs benpros")
fourthfit <- lm(formula = (y ~ cancer$benpros), data = cancer)
abline(fourthfit)
summary(fourthfit)

# plotting the vesinv
plot(x = cancer$vesinv, y = y, main = "Log(psa) vs vesinv")
fifthfit <- lm(formula = (y ~ cancer$vesinv), data = cancer)
abline(fifthfit)
summary(fifthfit)

# plotting the capspen
plot(x = cancer$capspen, y = y, main = "Log(psa) vs capspen")
sixthfit <- lm(formula = (y ~ cancer$capspen), data = cancer)
abline(sixthfit)
summary(sixthfit)

# plotting gleason
gleason_dums <- lapply(unique(cancer$gleason), function(g) as.numeric(cancer$gleason == g))
cancer$gleason6 <- unlist(gleason_dums[1])
cancer$gleason7 <- unlist(gleason_dums[2])
cancer$gleason8 <- unlist(gleason_dums[3])
cancer$gleason <- NULL

plot(x = cancer$gleason6, y = y, main = "Log(psa) vs gleason6")
gleason6fit <- lm(formula = (y ~ cancer$gleason6), data = cancer)
abline(gleason6fit)
summary(gleason6fit)

plot(x = cancer$gleason7, y = y, main = "Log(psa) vs gleason7")
gleason7fit <- lm(formula = (y ~ cancer$gleason7), data = cancer)
abline(gleason7fit)
summary(gleason7fit)

plot(x = cancer$gleason8, y = y, main = "Log(psa) vs gleason8")
gleason8fit <- lm(formula = (y ~ cancer$gleason8), data = cancer)
abline(gleason8fit)
summary(gleason8fit)

# all attributes together
firstcombinefit <- lm(formula = y ~ cancervol + weight + benpros + vesinv + capspen + gleason6 + gleason8, data = cancer)
summary(firstcombinefit)

# another combination
secondcombinefit <- lm(formula = y ~ cancervol + vesinv + gleason8, data = cancer)
summary(secondcombinefit)
anova(firstcombinefit,secondcombinefit)

# third combination
thirdcombinefit <- lm(formula = y ~ cancervol + vesinv + gleason8 + benpros, data = cancer)
summary(thirdcombinefit)
anova(firstcombinefit,thirdcombinefit)

# AIC results
allforwardcombine <- step(object = lm(formula = y ~ 1, data = cancer),scope = list(upper = ~ cancervol + weight + benpros + vesinv + capspen + gleason6 + gleason8), direction = "forward")
allbackwardcombine <- step(object = lm(formula = y ~ cancervol + weight + benpros + vesinv + capspen + gleason6 + gleason8, data = cancer), scope = list(lower = ~ 1), direction = "backward")
allbothcombine <- step(object = lm(formula = y ~ 1, data = cancer), scope = list(lower = ~ 1, upper = ~ cancervol + weight + benpros + vesinv + capspen + gleason6 + gleason8), direction = "both")

# Residual Plots
plot(fitted(thirdcombinefit), resid(thirdcombinefit))
abline(h = 0)

plot(fitted(thirdcombinefit), abs(resid(thirdcombinefit)))

# line series plot
plot(resid(thirdcombinefit), type = "l")
abline(h=0)

# QQ plots
qqnorm(resid(thirdcombinefit))
qqline(resid(thirdcombinefit))

# PSA prediction
cancervolL <- mean(cancer$cancervol)
weight <- mean(cancer$weight)
vesinv <- mean(cancer$vesinv)
gleason8 <- mean(cancer$gleason8)
benpros <- mean(cancer$benpros)
prediction <- predict(object = thirdcombinefit, newdata = data.frame(cancervol = cancervolL, vesinv = vesinv, gleason8 = gleason8, benpros = benpros))
print(sprintf("abs(Actual - Prediction) = %f", abs(exp(mean(y) - exp(prediction)))))

# END OF QUESTION 1

