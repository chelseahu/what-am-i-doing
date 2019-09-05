##### CODING CHALLENGES - PROJECT EULER #25

recursive_fib <- function(x) {
  if (x <= 1) {
    x
  } else {
    recursive_fib(x - 1) + recursive_fib(x - 2)
  }
}

# Too expensive for this problem

# FLAG: incomplete
