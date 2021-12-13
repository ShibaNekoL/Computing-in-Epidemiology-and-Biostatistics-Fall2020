## EX8

# pass
mid <- c(11,16,23,31,36,47,50)
fin <- c(3,9,16,20,27,31,36,49,50)

# odd
boy <- seq(1,50,2)
# even
girl <- seq(2,50,2)

# boys who passed both the midterm and final exams
intersect(intersect(mid, fin), boy)
# girls who passed both the midterm and final exams
intersect(intersect(mid, fin), girl)
# boys who passed the midterm exam but failed the final exam
intersect(setdiff(mid, fin), boy)
# girls who failed the midterm exam but passed the final exam
intersect(setdiff(fin, mid), girl)

## EX9

seizure <- read.csv(file.choose())

reg <- function(y, x){
  b1 <- sum((x - mean(x)) * (y - mean(y))) / (sum((x - mean(x)) ^ 2))
  b0 <- mean(y) - b1 * mean(x)
  return(c(b0, b1))
}

reg(seizure$y, seizure$ltime)
lm(seizure$y ~ seizure$ltime)