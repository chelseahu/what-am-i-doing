# This is just random test code for training/practice purposes

#install.packages("pacman")
pacman::p_load(tidyverse, modelr, lubridate, primes, sfsmisc, nycflights13)

##### TOPIC: spread() and gather()

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)

# Spread and gather are not symmetrical because variable types are lost in conversion

table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

# To set numbers (non-syntactic) as col names, use `` or ""

people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods", "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)
people2 <- people %>%
  group_by(name, key) %>%
  mutate(obs = row_number())

# The remaining cols (not key or value) must uniquely id a row. Can add another col to do this

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg_tidy <- preg %>% gather(male, female, key = "gender", value = "n", na.rm = T)
preg_tidy2 <- preg_tidy %>%
  mutate(
    female = gender == "female",
    pregnant = pregnant == "yes"
  ) %>%
  select(female, pregnant, n)

# Recast categories as logicals when possible for ease of cleaning and memory save

##### TOPIC: for loops

#1
mtcars_m <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  mtcars_m[i] <- mean(mtcars[[i]])
}
mtcars_m

#2

flights_type <- vector("list", ncol(nycflights13::flights))
names(flights_type) <- names(flights)
for (i in names(flights)) {
  flights_type[[i]] <- class(flights[[i]])
}
flights_type

# Test for whether for loop counts length of variable automatically when looping (i.e., do I have to set length(x) to loop through x)
x <- c(1, 2, 3, 4)
y <- vector("integer", 4)
for (i in x) {
  y[i] <- x[i] + 1
}
y

##### TOPIC: the map functions in package purrr

#1.1
mtcars %>% map_dbl(mean)

#1.2
nycflights13::flights %>% map_chr(typeof)

#1.3
iris %>% map(unique) %>% map_int(length)
#or
iris %>% map_int(function(x) length(unique(x)))
#or
iris %>% map_int(~ length(unique(.)))

#1.4
r_list <- c(-10, 0, 10, 100)
r_list %>% map(function(x) rnorm(n = 10, mean = x))
#or
r_list %>% map(~ rnorm(n = 10, mean = .))

#2
df %>% map_lgl(is.factor)

#3
#Takes in a vector but returns a list

#4
#map_bdl outputs a vector the length of the list or vector inputed
#Can use flatten() to collapse outputed list from map() to vector

##### TOPIC: modeling





##### CODING CHALLENGES - PROJECT EULER #1
##### See https://projecteuler.net/index.php?section=problems

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

##### CODING CHALLENGES - PROJECT EULER #2

# Solution 1
recursive_fib <- function(x) {
  if (x <= 2) {
    x
  } else {
    recursive_fib(x - 1) + recursive_fib(x - 2)
  }
}

gen_fib <- function(max) {
  fib_vec <- c(0)
  
  for (i in 1:50) {
    if (tail(fib_vec, 2)[-2] + last(fib_vec) < max) {
      fib_vec <- c(fib_vec, recursive_fib(i))
    }
  }
  
  fib_vec[-1]
}

r <- gen_fib(4000000)
sum(r[r %% 2 == 0])

# Solution 2
euler_2a <- function(x) {
  vals <- c(1, 2)
  
  for (i in 3:x) {
    newval <- vals[i - 1] + vals[i - 2]
    if (newval > x) {
      break
    } else {
      vals <- c(vals, newval)
    }
  }
  
  sum(vals[vals %% 2 == 0])
}
euler_2a(4000000)

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

##### CODING CHALLENGES - PROJECT EULER #4

find_max_pal <- function(x) {
  min_val <- 10 ^ (x - 1)
  max_val <- (10 ^ x) - 1
  
}

# FLAG: incomplete

##### CODING CHALLENGES - PROJECT EULER #5

# Upper bar is factorial(20)

# FLAG: incomplete

##### CODING CHALLENGES - PROJECT EULER #6

find_diff <- function(x) {
  sum_sq <- 1:x %>% map_dbl(~ .x^2) %>% sum
  sq_sum <- sum(1:x)^2
  abs(sum_sq - sq_sum)
}

1:100 %>% map_dbl(find_diff)

##### CODING CHALLENGES - PROJECT EULER #7

find_n_prime <- function(x) {
  out <- c()
  
  i <- 2
  
  while (length(out) <= (x - 1)) {
    if (is_prime(i)) {
      out <- c(out, i)
    }
    i <- i + 1
  }
  
  last(out)
}

##### CODING CHALLENGES - PROJECT EULER #8

# FLAG: incomplete

##### CODING CHALLENGES - PROJECT EULER #9

is_pyth_trip <- function(v) { # Enter vector of length 3
  a <- v[1]
  b <- v[2]
  c <- v[3]
  a^2 + b^2 == c^2
}

n <- as_tibble(combn(1:1000, 3)) # Too slow, not a feasible solution
pyth_trip_check <- n %>% map_lgl(is_pyth_trip) %>% as.vector
only_pyth <- n[c(pyth_trip_check)] 

find_sp_pyth <- function(t) { # Input is a tibble
  for (i in 1:ncol(t)) {
    if (sum(t[i]) == 1000) {
      return(t[i])
    }
  } 
}

find_sp_pyth(only_pyth)

# FLAG: incomplete

##### CODING CHALLENGES - PROJECT EULER #10-15

# FLAG: incomplete

##### CODING CHALLENGES - PROJECT EULER #16

str_split(as.character(2^1000), "") %>% map(as.integer) %>% unlist %>% sum

##### CODING CHALLENGES - PROJECT EULER #17

# install.packages("english")
# library(english)

ret <- vector("character", 1000) # Initialize empty list of length 1000

for (i in 1:1000) {
  eng <- gsub(" ","", as.english(i))
  ret[i] <- eng
}

paste0(ret, collapse = "") %>% str_length # Excludes "and"s because of "english" package

##### CODING CHALLENGES - PROJECT EULER #18-19

# FLAG: incomplete

##### CODING CHALLENGES - PROJECT EULER #20

factorial(100) %>% 
  as.character %>% 
  str_split("") %>% 
  map(as.integer) %>% 
  unlist %>% 
  sum

##### CODING CHALLENGES - PROJECT EULER #21


  















