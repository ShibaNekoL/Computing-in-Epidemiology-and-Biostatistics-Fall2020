## 1
# loglikelihood function
logLik <- function(X, lambda){
    loglikelihood <- 0
    for(i in 1:length(X)){
        loglikelihood <- loglikelihood + log(exp(- lambda) * (lambda ^ X[i]) / factorial(X[i]))
    }
    return(loglikelihood)
}

X <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
lambda <- seq(0, 10, 0.01)
fp2 <- logLik(X, lambda)
plot(lambda, fp2, col=1, type="l")

lambdahat <- sum(X) / length(X) # mean(X)
abline(v=lambdahat)

## 2
newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
    x <- x0       # x0: the initial value
    fx <- ftn(x)     
    iter <- 0
    while ((abs(fx[1]) > tol) & (iter < max.iter)) {
        x <- x - fx[1]/fx[2]
        fx <- ftn(x)
        iter <- iter + 1
        cat("At iteration", iter, "value of x is:", x, "\n")
    }
    if (abs(fx[1]) > tol) {
        cat("Algorithm failed to converge\n")
        return(NULL)
    } else {  # abs(fx[1]) <= tol
        cat("Algorithm converged\n")
        return(x)
    }
}

ftn <- function(lambda){
    f <- (-length(X)) + sum(X) / lambda
    df <- -(lambda ^ (-2)) * sum(X)
    return(c(f, df))
}

X <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
lambda <- seq(0, 10, 0.01)
f <- (-length(X)) + sum(X) /lambda
plot(lambda, f, type="l")

abline(h=0, col=2)

newtonraphson(ftn, 2, 1e-06)

# plot
X <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
lambda <- seq(0, 10, 0.01)
fp2 <- logLik(X, lambda)
plot(lambda, fp2, col=1, type="l")
abline(v=4.333333)




## 3
data <- read.csv(file.choose())

data$SEX <- ifelse(data$SEX=="M", 1, 0)
X <- cbind(rep(1, length(data$BMI3)), data$Treatment, data$AGE, data$SEX)
head(X)
beta <- solve(t(X) %*% X) %*% t(X) %*% matrix(data$BMI3, ncol=1)
beta
re <- data$BMI3 - X %*% beta
head(re)
sigma2 <- sum(re^2) / length(data$BMI3)
sigma2
-(length(data$BMI3) / 2) * (log(2 * pi) + log(sigma2) + 1)

model1 <- lm(BMI3 ~ Treatment + AGE + SEX, data=data)
logLik(model1)
