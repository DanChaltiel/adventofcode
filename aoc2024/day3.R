
library(tidyverse)

x = read_lines("aoc2024/day3_input.txt") %>% 
  paste(collapse="")

x = str_sub(x, 1, 500)
#part 1
a = x %>% 
  str_match_all("mul\\((\\d+),(\\d+)\\)") %>% 
  .[[1]]
sum(as.numeric(a[,2])*as.numeric(a[,3]))


#part 2
a = x %>% 
  str_split_1("(?=do(n't)?\\(\\))") %>% 
  discard(~str_starts(.x, "don't()")) %>% 
  paste(collapse="") %>% 
  str_match_all("mul\\((\\d+),(\\d+)\\)") %>% 
  .[[1]]
sum(as.numeric(a[,2])*as.numeric(a[,3]))
