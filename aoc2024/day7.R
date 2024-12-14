library(tidyverse)

x = read_lines("aoc2024/day7_input.txt")
x = read_lines(
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

permute = function(operators, n_spaces){
  p = gtools::permutations(length(operators), r=n_spaces, v=operators, 
                           set=FALSE, repeats.allowed=TRUE)
  lapply(seq_len(nrow(p)), function(i) p[i,])
}

# funcs = c(add=`+`, mult=`*`)
# funcs$add(1,1)
operators = c("+", "*")
get_fun = function(x){
  if(x=="+") return(`+`) 
  if(x=="|") return(paste0) 
  return(`*`)
}
line = x[5]

rtn = x %>% 
  map_dbl(.progress=TRUE, function(line){
    rslt = str_extract(line, "^(\\d+):", group=TRUE) %>% as.numeric()
    
    numbers = str_extract(line, ": (.*)", group=TRUE) %>% str_split_1(" ") %>% as.numeric()
    p = permute(operators, length(numbers)-1)
    a = p %>% 
      map_dbl(~{
        accumulate2(numbers, .x, \(acc, next_n, next_f){
          get_fun(next_f)(acc, next_n)
        }) %>% last()
      })
    
    if(any(a==rslt)) rslt else 0
})
sum(rtn) #932137732557


# Part 2 --------------------------------------------------------------------------------------

operators = c("+", "*", "|")

rtn = x %>% 
  map_dbl(.progress=TRUE, function(line){
    rslt = str_extract(line, "^(\\d+):", group=TRUE) %>% as.numeric()
    
    numbers = str_extract(line, ": (.*)", group=TRUE) %>% str_split_1(" ") %>% as.numeric()
    p = permute(operators, length(numbers)-1)
    a = p %>% 
      map_dbl(~{
        accumulate2(numbers, .x, \(acc, next_n, next_f){
          get_fun(next_f)(acc, next_n) %>% as.numeric()
        }) %>% last()
      })
    
    if(any(a==rslt)) rslt else 0
  })
sum(rtn) #661823605105500

#took 1 hour...
