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