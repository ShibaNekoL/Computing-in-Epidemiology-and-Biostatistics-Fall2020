prime_func <- function(testno){
  done <- FALSE
  i <- 2
  
  while(! done){
    if(testno %% i == 0){
      prime <- 0
      done <- T
    }
    
    if(i == testno){
      prime <- 1
      done <- T
    }
    
    i <- i + 1
  }
  return(prime)
}


prime_list <- c()

for(i in 2:100){
  
  if(prime_func(i) == 1){
    prime_list <- c(prime_list, i)
  }
}

prime_list
