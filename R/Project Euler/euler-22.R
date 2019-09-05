##### CODING CHALLENGES - PROJECT EULER #22

names <- read_delim("./Projects/R/Project Euler/euler-22-names.txt", delim = ",", col_names = F)

cln_names <- names %>% 
  select_if(function(x) !(is.na(x))) %>% # Removed cols with NAs
  unlist(use.names = F) %>% 
  sort

get_alph_val <- function(name) {
  by_letter <- name %>% 
    str_split(pattern = "") %>% 
    unlist
  alph_val <- by_letter %>% 
    match(LETTERS) %>% 
    sum
  alph_val
}

cln_names %>% map_int(~ get_alph_val(.) * match(., cln_names)) %>% sum

# Answer is incorrect because NAs are removed instead of kept in place