##### CODING CHALLENGES - PROJECT EULER #21

#install.packages("pacman")
#pacman::p_load(tidyverse, modelr, lubridate, primes, sfsmisc)

# Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
# If d(a) = b and d(b) = a, where a ??? b, then a and b are an amicable pair and each of a and b are called amicable numbers.

n <- 1:10000

check_primes <- map_lgl(n, is_prime)
no_primes <- n[!c(check_primes)]

all_comb <- combn(no_primes, 2)
