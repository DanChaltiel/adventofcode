



library(tidyverse)

# https://adventofcode.com/2024/day/2
x = read_lines("aoc2024/day2_input.txt")

test = function(x){
  x = (x-lag(x))[-1]
  same_sign = length(unique(sign(x))) == 1
  diff_ok = all(abs(x)>=1 & abs(x)<=3)
  same_sign && diff_ok
}

x %>% 
  map_lgl(~{
    a = .x %>% 
      str_split_1(" ") %>% 
      as.numeric()
    # b = (a-lag(a))[-1]
    test(a)
  }) %>% 
  sum()
#472

# https://adventofcode.com/2024/day/2#part2


x %>% 
  map_lgl(~{
    a = .x %>% 
      str_split_1(" ") %>% 
      as.numeric()
    
    test0 = test(a)
    tests = seq_along(a) %>% 
      map_lgl(function(i){
        test(a[-i])
      })
    any(c(test0, tests))
  }) %>% 
  sum()



# Test  ---------------------------------------------------------------------------------------


x = read_lines("
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
