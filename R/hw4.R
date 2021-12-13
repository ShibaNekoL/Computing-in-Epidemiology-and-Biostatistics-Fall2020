## EX S1
# for loops
factorial <- function(x){
  f <- 1
  for(i in 1:x){
    f <- f * i
  }
  return(f)
}

factorial(10)

# while loops
factorial <- function(x){
  f <- 1
  i <- 1
  done <- F
  while(!done){
    if(i == x){
      done = T
    }
    f <- f * i
    i <- i + 1
  }
  return(f)
}

factorial(10)


## EX 12


miss <- function(x){
  done <- F
  i <- 1
  while(!done){
    if(i == length(x)){
      done <- T
    }
    if(is.na(x[i]) == T){
      done <- T
      output <- i
    } 
    i <- i + 1
  }
  return(output)
}

x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)

cat(miss(x), "\n") 



## EX 15

bmi <- read.csv(file.choose())

x <- seq(0,9,3)
y <- cbind(bmi$BMI0, bmi$BMI1, bmi$BMI2, bmi$BMI3)

par(mfrow = c(1,2))

# plot 1
plot(x,y[1,],type="b",lwd=1,col=1,lty=1,pch=1,ylim=c(15,50),axes = F,xlab="months",ylab="BMI",main="Placebo group")
axis(1, at = x, labels = seq(0,9,3))
axis(2)
for(subj in 2:10){
  lines(x,y[subj,],lty=1,lwd=1,col=subj,type="b",pch=subj)
}
legend("topright",bty="n", c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"),lty=1,col=(1:10),lwd=1,pch=(1:10))

# plot 2
plot(x,y[51,],type="b",lwd=1,col=1,lty=1,pch=1,ylim=c(15,50),axes = F,xlab="months",ylab="BMI",main="Drug group")
axis(1, at = x, labels = seq(0,9,3))
axis(2)
for(subj in 52:60){
  lines(x,y[subj,],lty=1,lwd=1,col=subj,type="b",pch=subj)
}
legend("topright",bty="n", c("ID51","ID52","ID53","ID54","ID55","ID56","ID57","ID58","ID59","ID60"),lty=1,col=(1:10),lwd=1,pch=(1:10))


## EX S2
install.packages("plotrix")
library(plotrix)

subject <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")

pct <- round(subject/sum(subject)*100)
lbls <- paste(lbls, pct) # add percentages to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie3D(subject,labels=lbls,explode=0.1,main="Pie Chart of Countries with percentages")
