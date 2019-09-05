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