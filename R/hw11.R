# Ex 22-1
betaco <- c(-6,1.0,0.005)
n <- seq(30,630,200)
no.rep <- 1000
tol = 1e-9
n.max = 100
meanMLE <- matrix(NA,length(n),3)
meanMLEglm <- matrix(NA,length(n),3)
allMLE <- array(NA,c(no.rep,3,length(n)))
for(n.loop in 1: length(n)){
    MLE <- matrix(NA,no.rep,3)
    MLEglm <- matrix(NA,no.rep,3)
    for(i in 1:no.rep){
        set.seed(i)
        gpa <- rnorm(n[n.loop],3.1,0.3)

gre <- rnorm(n[n.loop],580,80)
x.beta <- betaco[1]+betaco[2]*gpa+betaco[3]*gre
pi.admit <- exp(x.beta)/(1+exp(x.beta))
Y <- c()
for(j in 1:n[n.loop]){
    Y[j] <- sample(c(0,1),1,c(1-pi.admit[j],pi.admit[j]),replace=F)
}
X <- cbind(rep(1,length(Y)),gpa,gre)

betacoef <- c(0,0,0)
pi1 <- exp(X %*% betacoef)/(1+exp(X %*% betacoef))
gradient <- t(X) %*% (Y-pi1)
hessian <- -t(X) %*% diag(c(pi1*(1-pi1)),length(Y)) %*% X
n.iter <- 0
while ((max(abs(gradient)) > tol) & (n.iter < n.max)) {
    betacoef <- betacoef - solve(hessian) %*% gradient
    pi1 <- exp(X %*% betacoef)/(1+exp(X %*% betacoef))
    gradient <- t(X) %*% (Y-pi1)
    hessian <- -t(X) %*% diag(c(pi1*(1-pi1)),length(Y)) %*% X
    n.iter <- n.iter + 1
}
if (n.iter == n.max) {
    cat('newton failed to converge\n')
} else {
    MLE[i,] <- betacoef
}
MLEglm[i,] <- glm(Y~gpa+gre,family=binomial)$coef
}
meanMLE[n.loop,] <- colSums(MLE)/no.rep
meanMLEglm[n.loop,] <- colSums(MLEglm)/no.rep
allMLE[,,n.loop] <- MLE
}

bias <- rbind(meanMLE[1,]-betaco, meanMLE[2,]-betaco, meanMLE[3,]-betaco, meanMLE[4,]-betaco)

rownames(meanMLE) <- c("n=30","n=230","n=430","n=630")
rownames(meanMLEglm) <- c("n=30","n=230","n=430","n=630")
rownames(bias) <- c("n=30","n=230","n=430","n=630")

meanMLE
meanMLEglm
bias

par(mfrow = c(3,1))
x <- seq(1,4,1)
boxplot(allMLE[,1,1],allMLE[,1,2],allMLE[,1,3],allMLE[,1,4], xaxt = 'n')
abline(h=betaco[1],col=2)
axis(1, at = x, labels = c("n=30","n=230","n=430","n=630"))
boxplot(allMLE[,2,1],allMLE[,2,2],allMLE[,2,3],allMLE[,2,4], xaxt = 'n')
abline(h=betaco[2],col=2)
axis(1, at = x, labels = c("n=30","n=230","n=430","n=630"))
boxplot(allMLE[,3,1],allMLE[,3,2],allMLE[,3,3],allMLE[,3,4], xaxt = 'n')
abline(h=betaco[3],col=2)
axis(1, at = x, labels = c("n=30","n=230","n=430","n=630"))
