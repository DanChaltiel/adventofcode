# https://adventofcode.com/2024/day/1


library(tidyverse)
x = read_delim("day1_input.txt", delim="   ", col_names=FALSE)
sum(abs(sort(x$X1)-sort(x$X1)))


# https://adventofcode.com/2024/day/1#part2


x = read_delim("
3   4
4   3
2   5
1   3
3   9
3   3", delim="   ", col_names=FALSE)

map_dbl(x$X1, ~.x*sum(x$X2==.x)) %>% sum()
