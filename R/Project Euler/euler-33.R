##### CODING CHALLENGES - PROJECT EULER #33

#library(rlist)

# Only two digits in n and d

perms <- expand.grid(10:99, 10:99)

check <- vector("list", nrow(perms))

for (i in 1:nrow(perms)) {
  
  n <- perms[i, 1] %>% 
    as.character %>% 
    str_split(pattern = "") %>% 
    unlist
  
  d <- perms[i, 2] %>% 
    as.character %>% 
    str_split(pattern = "") %>% 
    unlist
  
  if ((n[1] %in% d | n[2] %in% d) & n[2] != "0" & d[2] != "0" & perms[i, 1] < perms[i, 2]) {
    check[[i]] <- perms[i, ] %>% as.integer
  }
}

check <- check[!sapply(check, is.null)]

result <- c()

for (i in 1:length(check)) {
  
  frac <- check[[i]][1] / check[[i]][2]
  
  n <- check[[i]][1] %>% 
    as.character %>% 
    str_split(pattern = "") %>% 
    unlist %>% 
    as.integer
  
  d <- check[[i]][2] %>% 
    as.character %>% 
    str_split(pattern = "") %>% 
    unlist %>% 
    as.integer
  
  keep <- c(setdiff(n, d), setdiff(d, n))
  
  if (length(keep) > 1) {
    w_frac <- keep[1] / keep[2]
    
    if (frac == w_frac) {
      result <- c(result, check[[i]])
    }
  } else {
    
    keep2 <- c()
    
    if (n[1] == d[1]) {
      keep2 <- c(n[2], d[2])
    } else {
      keep2 <- c(n[1], d[1])
    }
    
    w_frac <- keep2[1] / keep2[2]
    
    if (frac == w_frac) {
      result <- c(result, check[[i]])
    }
  }
}

result


