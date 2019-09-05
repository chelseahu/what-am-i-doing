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