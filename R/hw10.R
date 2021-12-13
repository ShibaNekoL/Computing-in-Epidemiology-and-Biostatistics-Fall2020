p <- 0.15
n <- 20
no.rep <- 1000
l95 <- rep(NA,no.rep)
r95 <- rep(NA,no.rep)

for(i in 1:no.rep){
    #print(i)
    set.seed(i)
    x <- rbinom(1,size=n,prob=p)
    phat <- x/n
    l95[i] <- max(0,phat-qnorm(0.975)*sqrt(phat*(1-phat)/n))
    r95[i] <- phat+qnorm(0.975)*sqrt(phat*(1-phat)/n)
}
mean((l95<=p) & (p<=r95))



newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100){
    x <- x0 #先把xassign為起始值x0
    fx <- ftn(x)
    iter <- 0
    while ((abs(fx[1]) > tol) && (iter < max.iter)){
        x <- x - fx[1]/fx[2]
        fx <- ftn(x)
        iter <- iter + 1
        #cat("At iteration", iter, "value of x isะ", x, "\n")
    }
    if (abs(fx[1]) > tol){
        cat("Algorithm failed to converge\n")
        return(NULL)
    } else{
        #cat("Algorithm converged\n")
        return(x)
    }
}



findECI <- function(datapoint,coverage){
   ftn7 <- function(p){
   fp <- (1-coverage)/2-1
   dfp <- 0
   for(k in 0:(datapoint-1)){ ### the datapoint should be >= 1
   fp <- fp+choose(20,k)*(p^k)*((1-p)^(20-k))
   dfp <- dfp+choose(20,k)*((k*p^(k-1))*((1-p)^(20-k))-(p^k)*((20-k)*(1-p)^(19-k)))
   }
   return(c(fp, dfp))
}
if(datapoint<5){
   pl <- newtonraphson(ftn7, 0.03, 1e-09)
}
if(datapoint>=5 & datapoint<8){
   pl <- newtonraphson(ftn7, 0.10, 1e-09)
}
if(datapoint>=8){
   pl <- newtonraphson(ftn7, 0.20, 1e-09)
}


ftn8 <- function(p){
    fp <- -(1-coverage)/2
    dfp <- 0
    for(k in 0:datapoint){
        fp <- fp+choose(20,k)*(p^k)*((1-p)^(20-k))
        dfp <- dfp+choose(20,k)*((k*p^(k-1))*((1-p)^(20-k))-(p^k)*((20-k)*(1-p)^(19-k)))
    }
    return(c(fp, dfp))
}
if(datapoint<5){
    pu <- newtonraphson(ftn8, 0.3, 1e-09)
}
if(datapoint>=5 & datapoint<8){
    pu <- newtonraphson(ftn8, 0.5, 1e-09)
}
if(datapoint>=8){
   pu <- newtonraphson(ftn8, 0.6, 1e-09)
}
   return(c(pl,pu))
}



le95 <- rep(NA,no.rep)
re95 <- rep(NA,no.rep)

for(i in 1:no.rep){
    #print(i)
    set.seed(i)
    x <- rbinom(1,size=n,prob=p)
    le95[i]<- findECI(x,0.95)[1]
    re95[i]<- findECI(x,0.95)[2]
    
    if(x==0){
        le95[i] <- 0
    }
}
mean((le95<=p)&(p<=re95))
mean(r95-l95,na.rm=T)
mean(re95-le95,na.rm=T)



x <- c()
for(i in 1:no.rep){
    #print(i)
    set.seed(i)
    x[i]<- rbinom(1,size=n,prob=p)
}
aa <- cbind(x,l95,r95,le95, re95)
xx<-9
aa[aa[,1]==xx,]



# library(binom)
# binom.confint(xx, n, conf.level = 0.95, methods = "asymptotic")
# binom.confint(xx, n, conf.level = 0.95, methods="exact")
