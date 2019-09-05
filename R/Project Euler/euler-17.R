##### CODING CHALLENGES - PROJECT EULER #17

# install.packages("english")
# library(english)

ret <- vector("character", 1000) # Initialize empty list of length 1000

for (i in 1:1000) {
  eng <- gsub(" ","", as.english(i))
  ret[i] <- eng
}

paste0(ret, collapse = "") %>% str_length # Excludes "and"s because of "english" package