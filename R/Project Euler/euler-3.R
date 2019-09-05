##### CODING CHALLENGES - PROJECT EULER #3

find_prime_fs <- function(x) {
  r <- c()
  
  for (i in 1:x) {
    if (x%%i == 0 && is_prime(i)) {
      r <- c(r, i)
    }
  }
  
  r
}

max(find_prime_fs(600851475143)) # Fails for numbers that are too big

# FLAG: incomplete