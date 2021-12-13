### WEEK1

## logistic model and summary
oo <- glm(admit ~ gre + gpa, data=aa, family=binomial)
summary(oo)

## showing
options(digits=16)

## create sequence
seq(from=,to=,by=)

##
demo(graphics)

## normal distribution
x <- seq(-3, 3, 0.01)
y <- dnorm(x, mean=0, sd=1)
plot(x, y)

y[1]
dnorm(-3, 0, 1) # 0 1  normal distribution

# plot in various style
plot(x, y, type="l", lty=2, lwd=3) # type = line


## probability of statistic number
pnorm(0)
pnorm(1.96, lower=F)
1- pnorm(1.96)

## from probability to statistic
qnorm(0.025)

## random pick 10 numbers, mean=5
rnorm(10, 5, 3)

## chi square distribution
x <- seq(0, 10, 0.01)
y <- dchisq(x, 1)
plot(x, y)

## 
x <- seq(0, 10, 0.01)
y <- pchisq(x, 1)
plot(x, y)

## 
Y <- matrix(NA, 3, 2) # rol, col
matrix(c(1,2,3,4), 2, 2)
matrix(c(1,2,3,4), 2, 2, byrow=T)

# matrix multiply
A %*% B


# 
lm(y ~ trt + bline + age, data=Seizuei1)

x <- cbind(rep(1, nrow(Seizure1)), Seizure1$trt, Seizure1$bline, Seizure1$age)
solve(t(X) %*% X) %*% t(X) %*% matrix(Seizure1$y, ncol=1)


### WEEK2

# user-defined function

# missing data
is.na(x)

# sample
# 可重複取樣
sample(1:39, size=10, replace=T)

c(sort(sample(1:20, 9)), NA)

# 交集
intersect(x, y)
# 聯集
union(x, y)
#
setdiff(x, y) # in x but not in y
setdiff(y, x) # in y but not in x
setequal(x, y) # compare if x y is equal
is.element(x, y) = x%in%y 
is.element(y, x) = y%in%x


