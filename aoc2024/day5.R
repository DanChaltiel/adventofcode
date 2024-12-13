


library(tidyverse)

# https://adventofcode.com/2024/day/2
x = read_lines("aoc2024/day5_input.txt")
x = read_lines("47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

# rules = x %>% str_subset("\\|")
# ups =  x %>% str_subset(",")
i = which(x=="")
rules = x[1:i-1]
ups =  x[(i+1):length(x)]

rules2 = rules %>% str_split("\\|") %>% map(~set_names(as.list(as.numeric(.x)), c("inf", "sup")))

# up = "75,47,61,53,29"
# .x = rules2[[1]]

test_update = function(up){
  up2 = up
  if(is.character(up2)){
    up2 = str_split_1(up, ",") %>% as.numeric()
  }
  rules2 %>% 
    keep(~{
      all(.x %in% up2)
    }) %>% 
    map_lgl(~{
      match(.x$inf, up2) < match(.x$sup, up2)
    }) %>% 
    all()
}

ups %>% 
  keep(test_update) %>% 
  map_dbl(~{
    str_split_1(.x, ",") %>% as.numeric() %>% .[(length(.)+1)/2]
  }) %>% 
  sum() #7365



# Part 2 --------------------------------------------------------------------------------------

# up = "75,97,47,61,53"
# .x = rules2[[1]]

rule_swap1 = function(up, rules){
  for(i in rules){
    x1 = match(i$inf, up)
    x2 = match(i$sup, up)
    if(x1>x2){
      tmp = up[x1]
      up[x1] = up[x2]
      up[x2] = tmp
    }
  }
  up
}

correct_update = function(up){
  up2 = up
  if(is.character(up2)){
    up2 = str_split_1(up, ",") %>% as.numeric()
  }
  rules_up = rules2 %>% 
    keep(~{
      all(.x %in% up2)
    }) 
  # rules_up %>% bind_rows()
  r=0
  while(!test_update(up2)){
    if(r>length(rules_up)) browser()
    up2 = rule_swap1(up2, rules_up)
    r = r+1
  }
  paste(up2, collapse=",")
}

rslt2 = ups %>% 
  discard(test_update) %>% 
  map(correct_update, .progress=TRUE) 

rslt2 %>% 
  map_dbl(~{
    str_split_1(.x, ",") %>% as.numeric() %>% .[(length(.)+1)/2]
  }) %>% 
  sum() #5770
