newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
    x <- x0        # x0: the initial value
    fx <- ftn(x)
    iter <- 0
    while ((max(abs(fx[[1]])) > tol) & (iter < max.iter)) {
        x <- x - solve(fx[[2]]) %*% fx[[1]]
        fx <- ftn(x)
        iter <- iter + 1
    }
    if (max(abs(fx[[1]])) > tol) {
        cat('Algorithm failed to converge\n')
        return(NULL)
    } else {     # max(abs(fx[[1]])) <= tol
        cat("Algorithm converged\n")
        return(x)
    }
}

## 1

resp <- read.csv(file.choose(), header=T)

# change string levels into 1 and 0
resp$treatment <- ifelse(resp$treatment=="P", 1, 0)

X <- cbind(rep(1, length(resp$outcome)), resp$treatment, resp$age, resp$baseline)
dim(X)

Y <- resp$outcome
ftn <- function(betacoef){
    pi1 <- exp(X %*% betacoef) / (1 + exp(X %*% betacoef))
    gradient <- t(X) %*% (Y - pi1)
    hessian <- - t(X) %*% diag(c(pi1 * (1 - pi1)), length(resp$outcome)) %*% X
    return(list(gradient, hessian))
}

newtonraphson(ftn, c(0,0,0,0))


# check
# glm(outcome~treatment+age+baseline, family=binomial, data=resp)


## 2
beta <- newtonraphson(ftn, c(0,0,0,0))
beta
solve(-ftn(beta)[[2]])

# check
# model <- glm(outcome~treatment+age+baseline, family=binomial, data=resp)
# vcov(model)


## 3
ftn <- function(betacoef){
    pi1 <- exp(X %*% betacoef) / (1 + exp(X %*% betacoef))
    gradient <- t(X) %*% (Y - pi1)
    hessian <- - t(X) %*% diag(c(pi1 * (1 - pi1)), length(resp$outcome)) %*% X
    logLike <- sum(Y * log(pi1 / (1 - pi1)) + log(1 - pi1))
    return(list(gradient, hessian, logLike))
}

ftn(beta)[[3]]

# check
# logLik(model)
