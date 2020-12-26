# QUESTION 1
# import bootstrap library
library(boot)

# get the columns
gpact <- read.csv("gpa.csv", header = TRUE, sep = ",")
gradePointAvg = gpact[,1]
act = gpact[,2]

# bootstrap function
getCorrelation <- function(x,index){
  result <- cor(x[index,1],x[index,2])
  return(result)
}

# do a plot
plot(x=act,y=gradePointAvg,main = "gpa vs. act")

# do bootstrap
bootstrapR <- boot(gpact,getCorrelation,R=1000,sim = "ordinary", stype="i")

# summary statistics
plot(bootstrapR)
bootstrapR
mean(bootstrapR$t)
sd(bootstrapR$t)

# 95% CI
boot.ci(boot.out = bootstrapR)
# END OF QUESTION 1

# QUESTION 2
# read in data
volts <- read.csv("VOLTAGE.csv", header = TRUE, sep = ",")
volts0 <- volts[,2][1:30]
volts1 <- volts[,2][31:60]

# part a
boxplot(volts0,volts1, main = "Voltage readings of 2 locations", names = c("0","1"), ylab = "Voltage")
mean(volts0)
median(volts0)
mean(volts1)
median(volts1)

# part b
t.test(volts0,volts1)
# END OF QUESTION 2

# QUESTION 3
# read in file
vapors <- read.csv("VAPOR.csv", header = TRUE, sep = ",")
calcVapor <- vapors[,2]
observeVapor <- vapors[,3]

# exploratory analysis of the data
boxplot(calcVapor,observeVapor, main = "calculated vs. observed values of vapor pressure", ylab = "Vapor Pressure", names = c("theoretical","experimental"))

# confidence interval
t.test(calcVapor,observeVapor)

# more "manual" way to determine
# is a paired sample
mean(calcVapor - observeVapor)

# END OF QUESTION 3