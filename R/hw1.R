### EX2

## p-value of f-test
pf(3.2, df1=3, df2=194, lower=F) 

## plot pdf
x <- seq(0, 5, 0.01)
y <- df(x, 3, 194)
plot(x, y, type="l", lwd=2)

## plot cdf
x <- seq(0, 5, 0.01)
y <- pf(x, 3, 194)
plot(x, y, type="l", lwd=2)

### EX3

## p-value of t-test
(pt(-2.08, df=136)) * 2
(1 - pt(2.45, df=136)) * 2

## plot pdf
x <- seq(-5, 5, 0.01)
y <- dt(x, 136)
plot(x, y, type="l", lwd=2)

## plot cdf
x <- seq(-5, 5, 0.01)
y <- pt(x, 136)
plot(x, y, type="l", lwd=2)