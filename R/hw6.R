
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


## 1
p <- seq(0, 1, 0.01)
fp <- (-0.975)
for(k in 0:19){
    fp <- fp + choose(100,k)*(p^k)*((1-p)^(100-k))
}
plot(p, fp, type="l")

ftn7 <- function(x) {

    fx <- (- 0.975)
    dfx <- 0
    
    for(i in 0:19){
        fx <- fx + choose(100, i) * (x ^ i) * ((1 - x) ^ (100 - i))
        dfx <- dfx + choose(100, i) * (i * (x ^ (i - 1)) * ((1 - x) ^ (100 - i)) - (x ^ i)*(100 - i) * ((1 - x) ^ (99 - i)))
    }
    
    return(c(fx, dfx))
}

newtonraphson(ftn7, 0.1, 1e-06)


p <- seq(0, 1, 0.01)
fp <- (-0.025)
for(k in 0:20){
    fp <- fp + choose(100,k)*(p^k)*((1-p)^(100-k))
}
plot(p, fp, type="l")

ftn8 <- function(x) {
    
    fx <- (-0.025)
    dfx <- 0

    for(i in 0:20){
        fx <- fx + choose(100, i) * (x ^ i) * ((1 - x) ^ (100 - i))
        dfx <- dfx + choose(100, i) * (i * (x ^ (i - 1)) * ((1 - x) ^ (100 - i)) - (x ^ i) * (100 - i) * ((1 - x) ^ (99 - i)))
    }
    return(c(fx, dfx))
}

newtonraphson(ftn8, 0.3, 1e-06)

## 2
(20 / 100) - qnorm(0.975) * sqrt((20 / 100) * (1 - (20 / 100)) / 100)

(20 / 100) + qnorm(0.975) * sqrt((20 / 100) * (1 - (20 / 100)) / 100)
