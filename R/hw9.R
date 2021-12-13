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

rate <- read.csv(file.choose())

rate$Age.f <- factor(rate$Age)

# dummy
agedummy <- ifelse(rate$Age==2, 1, 0)
for(i in 3:12){
    agedummy <- cbind(agedummy, ifelse(rate$Age==i, 1, 0))
}

Y <- rate$Death
X <- cbind(rep(1,length(Y)),agedummy,ifelse(rate$sex=="m",1,0))
ftn <- function(betacoef){
    mu <- exp(X %*% betacoef + log(rate$PY / 100000))
    gradient <- t(X) %*% (Y - mu)
    Hessian <- -t(X) %*% diag(c(mu),length(Y)) %*% X
    logL <- sum(-mu + Y * log(mu) - log(factorial(Y)))
    return(list(gradient, Hessian,logL))
}
newtonraphson(ftn,c(0,0,0,0,0,0,0,0,0,0,0,0,0))

# model <- glm(Death~Age.f+sex,offset=log(PY/100000), data=rate, family=poisson)

#########################
beta <- newtonraphson(ftn,c(0,0,0,0,0,0,0,0,0,0,0,0,0))
solve(-ftn(beta)[[2]])

# vcov(model)

#########################

ftn(beta)[[3]]

# logLik(model)