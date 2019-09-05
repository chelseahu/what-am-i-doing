##### CODING CHALLENGES - PROJECT EULER #20

factorial(100) %>% 
  as.character %>% 
  str_split("") %>% 
  map(as.integer) %>% 
  unlist %>% 
  sum