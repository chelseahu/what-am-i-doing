##### See https://projecteuler.net/index.php?section=problems

##### CODING CHALLENGES - PROJECT EULER #1

# Solution 1
mul <- 0
for (i in 1:99) { # Below 100, non-inclusive
  if (i%%3 == 0 | i%%5 == 0) {
    mul = mul + i 
  }
}
mul

# Solution 2
euler_1 <- function(x) {
  vals <- 1:(x-1)
  sum(vals[vals%%3 == 0 | vals%%5 == 0])
}
euler_1(100)