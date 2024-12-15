
library(tidyverse)
Sys.setenv(LANGUAGE="en")
globalCallingHandlers(NULL)
rlang::global_entrace()

# source("aoc2024/utils.R")


# Functions -----------------------------------------------------------------------------------


even_ndig = function(x){
  nchar(x) %% 2 == 0
}

split = function(x){
  mid = nchar(x)/2
  c(substr(x, 1, mid), substr(x, mid+1, nchar(x))) %>% as.numeric()
}

# blink1 = function(x){
#   if(x==0) return(1)
#   if(even_ndig(x)) return(split(x))
#   return(as.numeric(x)*2024)
# }
blink1 = function(x){
  if(!is.null(cache[[as.character(x)]])) return(cache[[as.character(x)]])
  if(x==0) rtn = 1
  else if(even_ndig(x)) rtn = split(x)
  else rtn = as.numeric(x)*2024
  cache[[as.character(x)]] = rtn
  rtn
}

blink = function(x){
  x %>% map(blink1) %>% unlist()
}

if(!exists("cache")){
  # cache = rlang::new_environment()
  # ls(cache)
  cache = readRDS("day11_cache.rds")
}

# Part 1 --------------------------------------------------------------------------------------

input = "0 1 10 99 999"
input = "125 17"
input = read_lines("aoc2024/day11_input.txt")
# input = str_split_1(input, " ")



out = input %>% 
  str_split_1(" ")
# n_blink = 6
# n_blink = 25
n_blink = 75
# n_blink = 25
t0 = t1 = Sys.time()

if(!exists("l")){
  # l = list(out)
  l = readRDS("day11.rds")
}
last_i = length(l)
cli::cli_inform("Last index calculated: {last_i}, of length {length(l[[last_i]])}")

for(i in 2:(n_blink+1)){
  if(!lengths(l)[i] %in% c(NA, 0)){
    cli::cli_inform("Retreiving i={i}")
    next
  }
  l[[i]] = blink(l[[i-1]])
  cli::cli_inform("Calculated i={i}, took {format(Sys.time() - t1)}")
  t1 = Sys.time()
  saveRDS(l, "day11.rds")
  saveRDS(cache, "day11_cache.rds")
}

l[[26]] %>% length() %>% print() #203953 ! part 1 validated


#Can't do it by bruteforce :-(

l[[76]]




# l[lengths(l)==1] = NULL




# Essai ---------------------------------------------------------------------------------------

out %>% map(blink1)


