##### CODING CHALLENGES - PROJECT EULER #6

find_diff <- function(x) {
  sum_sq <- 1:x %>% map_dbl(~ .x^2) %>% sum
  sq_sum <- sum(1:x)^2
  abs(sum_sq - sq_sum)
}

1:100 %>% map_dbl(find_diff)