


library(tidyverse)

x = read_lines("aoc2024/day6_input.txt")
x = read_lines(
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

m_start = x %>% 
  str_split("") %>% 
  map(~matrix(.x, nrow=1)) %>% 
  do.call(rbind, .)

pos_start = which(m_start=="^", arr.ind = TRUE)

directions = lst(dir_up    = c(x=-1, y=0),
                 dir_right = c(x=0, y=1),
                 dir_down  = c(x=1, y=0),
                 dir_left  = c(x=0, y=-1))

mod_start = lst(m=m_start, pos=pos_start, dir=1)


one_step = function(mod){
  m=mod$m; pos=mod$pos; dir=mod$dir
  m[pos] = "X"
  d = directions[[dir]]
  new_pos = pos + directions[[dir]]
  new_val = try(m[new_pos], silent=TRUE)
  if(inherits(new_val, "try-error") || length(new_val)==0){
    return(lst(m, pos, dir=NULL))
  }
  if(new_val == "#") dir = (dir %% 4) + 1
  pos = pos+directions[[dir]]
  m[pos] = "^"
  lst(m, pos, dir)
}

walk_step = function(mod, n=1){
  for(i in seq(n)){
    mod = one_step(mod)
    # print(mod$dir)
    if(is.null(mod$dir)){
      print("Finish")
      return(mod)
    }
  }
  mod
}


m = mod_start %>% walk_step(9999)
m$m %>% 
  apply(2, function(x){
    as.numeric(case_when(x=="X" ~ 1, 
                         x=="#" ~ 2,
                         x=="." ~ 0,))
  }) %>% 
  image(useRaster=TRUE, axes=FALSE)

sum(m$m=="X")# 5212



# Part 2 --------------------------------------------------------------------------------------


