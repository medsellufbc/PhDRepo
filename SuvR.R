# Simulating linear model 
# Suppose we want to simulate from the following linear model
# y=b0+b1x+e, b0=0.5 and b1=2

set.seed(20)
x <-rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5+2*x+e
summary(y)
plot(x, y)

set.seed(1)
x <-rnorm(100)
e <- rnorm(100, 0, 2)
log.mu <- 0.5+0.3*x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y)

set.seed(10)
x <-rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5+2*x+e
summary(y)
plot(x, y)

library(survival)
library(urca)
mydata<-lung
mydata
## Step (1)
## Create the survival object
## we need status = 0 --> no event and status = 1 --> event happened
recodestatus <-function(x){
  if (x==1){rs=0} ## no event/ censored
  if (x==2){rs=1} ## event happened
  return(rs)
}
for (i in 1: length(mydata$status)) {
  mydata$recodedStatus[i]<-recodestatus(mydata$status[i])
}
mySurv <- Surv(time=mydata$time, event = mydata$recodedStatus)
class(mySurv)
head(mySurv)

## single survival curve: no comparisons in the dataset
myfit <-survfit(mySurv~1) ## single curve for all patient in the dataset
myfit
median(mydata$time)

### Median survival is the time at which the survivorship function equals 0.5
plot(myfit)
plot(myfit, conf.int = "none")
abline(h=0.5)
abline(v=310)

## specify predictor variable in the formula
myfit1<-survfit(mySurv~mydata$sex)
myfit1
plot(myfit1)
table(mydata$sex)

# 1 = Male, 2 = Female
plot(myfit1, col=c("red", "blue")) ## red = Male, Blue = female
plot(myfit1, col = c("red", "blue"))
plot(myfit1, col = c("red", "blue"), mark = 3) ## mark.time=T marked at 
## each censoring time
legend("topright", c("male","female"), col = c("red", "blue"), lty=1)
abline(h=0.5)
abline(v=270, col="blue")
abline(v=426, col="red")

#Now we see that the survival of female is better,
## Q: Is it better by chance, or statistically significant?
survdiff(mySurv~mydata$sex)

### plot the inverse of a survival function
plot(myfit1, fun="event", col=c("red", "blue"), mark=3)

coxph(mySurv ~ mydata$sex + mydata$age)

