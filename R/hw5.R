## EX2
# n個盤子

hanoi <- function(n, A, B, C){
    if(n == 1){
        return(c(A, C))  
    }else{
        return(c(hanoi(n - 1, A, C, B), hanoi(1, A, B, C), hanoi(n - 1, B, A, C)))
    }
}

n <- 20

length(hanoi(n, 'A', 'B', 'C')) / 2


## EX3
x <- matrix(c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000,
              3600, 4500, 10000, 8500, 3000, 10000, 1000, NA, 1200, 10000,
              3800, 5500, 9000, 6000, 6600, 3000, 9600, 6500, 8200, 8000,
              5000, 6600, 13000, 4500, 5000, NA, 10600, 9500, 7600, 6000,
              6600, 8000, 17000, 3000, 7000, 1000, 12600, 8500, 6000, NA),5,10, byrow = TRUE) 


col <- apply(x, 2, function(y){c(median(y, na.rm=T), max(y, na.rm=T), min(y, na.rm=T))})
row.names(col) <- c("medians", "maximums", "minimums")
colnames(col) <- c("col1", "col2", "col3", "col4", "col5", "col6", "col7", "col8", "col9", "col10")
col

row <- apply(x, 1, function(y){c(median(y, na.rm=T), max(y, na.rm=T), min(y, na.rm=T))})
row.names(row) <-c("medians", "maximums", "minimums") 
colnames(row) <- c("row1", "row2", "row3", "row4", "row5")
row


## EX8
A2 <- matrix(c(1,1,1,-3,-2,-1,1,3,1),3,3)
b2 <- matrix(c(4,6,4),3,1)
solve(A2,b2)