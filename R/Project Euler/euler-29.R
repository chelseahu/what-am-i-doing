##### CODING CHALLENGES - PROJECT EULER #29

a <- 2:100
b <- 2:100

perms <- expand.grid(a, b)

r <- vector("integer", nrow(perms))
  
for (i in 1:nrow(perms)) {
  r[i] <- perms[i, 1] ^ perms[i, 2]
}

r %>% unique %>% length 