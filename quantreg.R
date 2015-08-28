library(quantreg)

# vignette code
data("engel")
fit1 <- rq(foodexp ~ income, tau = .5, data = engel)
fit1
summary(fit1)
r1 <- resid(fit1)
c1 <- coef(fit1)

data(stackloss)
rq(stack.loss ~ stack.x,.5)  #median (l1) regression  fit for the stackloss data. 
rq(stack.loss ~ stack.x,.25)  #the 1st quartile, 
#note that 8 of the 21 points lie exactly on this plane in 4-space! 
rq(stack.loss ~ stack.x, tau=-1)   #this returns the full rq process
rq(rnorm(50) ~ 1, ci=FALSE)    #ordinary sample median --no rank inversion ci
rq(rnorm(50) ~ 1, weights=runif(50),ci=FALSE)  #weighted sample median 
#plot of engel data and some rq lines see KB(1982) for references to data

data(engel)
attach(engel)
plot(income,foodexp,xlab="Household Income",ylab="Food Expenditure",type = "n", cex=.5)
points(income,foodexp,cex=.5,col="blue")
taus <- c(.05,.1,.25,.75,.9,.95)
xx <- seq(min(income),max(income),100)
f <- coef(rq((foodexp)~(income),tau=taus))
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
  lines(xx,yy[,i],col = "gray")
}

lines(xx,yy[,2],col = "gray")

abline(lm(foodexp ~ income),col="red",lty = 2)
abline(rq(foodexp ~ income), col="blue")
legend("bottomright",c("mean (LSE) fit", "median (LAE) fit"),
       col = c("red","blue"),lty = c(2,1))
#Example of plotting of coefficients and their confidence bands
  plot(summary(rq(foodexp~income,tau = 1:19/20,data=engel), se="nid"), ols=TRUE)
#Example to illustrate inequality constrained fitting
n <- 100
p <- 5
X <- matrix(rnorm(n*p),n,p)
y <- .95*apply(X,1,sum)+rnorm(n)
#constrain slope coefficients to lie between zero and one
R <- cbind(0,rbind(diag(p),-diag(p)))
r <- c(rep(0,p),-rep(1,p))
rq(y~X,R=R,r=r,method="fnc")



# compare quantreg to qreg

autos <- read.csv("/Users/jcf2d/Box Sync/_statistics/autos.csv")
# qreg price weight length foreign, vce(robust)
q1 <- rq(price ~ weight + length + foreign, data=autos, tau = c(0.25,0.5,0.75))
summary(q1)
summary(q1, se="nid")
summary(q1, se="boot")


# these two produce the same graphs
# (1) Stata syntax
# import delimited "C:\Users\jcf2d\Box Sync\_statistics\engel.csv", clear 
# qreg foodexp income, vce(robust)
# grqreg, ci ols cons /*user installed package*/

# (2) R code
plot(summary(rq(foodexp~income,tau = 1:19/20,data=engel), se="nid"))

# some intuition for quantile regression
# not needed when errors are iid N(0,sigma)
x <- seq(1, 5, length.out = 100)
y <- 3 + 2*x + rnorm(100,sd=1.4)
plot(y ~ x)
abline(lm(y ~ x))
plot(summary(rq(y ~ x,tau = 1:19/20), se="nid"), parm="x")


plot(foodexp ~ income, data=engel)
abline(lm(foodexp ~ income, data=engel))
plot(summary(rq(foodexp~income,tau = 1:19/20,data=engel), se="nid"), par="income")


qout3 <- rq(stack.loss ~ stack.x, tau = 2)
plot(qout3)
# nicer plot method
qout4 <- rq(stack.loss ~ stack.x, tau = 1:19/20)
plot(qout4)


data("engel")
fm <- rq(foodexp ~ income, data = engel, tau = 1:9/10)
sfm <- summary(fm)
plot(fm)
plot(sfm)


summary(rq(foodexp ~ income, data = engel), se="iid")
summary(rq(foodexp ~ income, data = engel), se="nid")
summary(rq(foodexp ~ income, data = engel, method="br"), se="iid")


