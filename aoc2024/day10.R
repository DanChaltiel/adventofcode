
library(tidyverse)
Sys.setenv(LANGUAGE="en")
globalCallingHandlers(NULL)
rlang::global_entrace()

source("aoc2024/utils.R")

# Functions -----------------------------------------------------------------------------------

get_matrix = function(example=TRUE){
  input = 
    "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"
  if(!example) input = "aoc2024/day10_input.txt"
  
  read_matrix(input)
}

as_matrix = function(x){
  if(!is.matrix(x)) x = rbind(x)
  x
}

next_neighbours = function(coord, m){
  # browser()
  voisins = neighbours(coord, m)
  cur_val = m[rbind(coord[c("row", "col")])]
  ok_val = m[voisins] == cur_val+1
  # voisins[ok_val,] %>% structure(parent = coord)
  voisins[ok_val,]
}

neighbours = function(coord, m, diag=FALSE){
  stopifnot(length(coord)==2)
  coord = as_matrix(coord)
  if(diag){
    d = expand.grid(row=c(-1, 0, 1), col=c(-1, 0, 1)) %>% as.matrix()
  } else {
    d = expand.grid(row=c(-1, 0, 1), col=c(-1, 0, 1)) %>% 
      filter(row==0 | col==0) %>% 
      as.matrix()
  }
  if(nrow(coord)==1){
    coord =  coord[rep(1, nrow(d)),]
  }
  # browser()
  coord2 = coord + d
  ok = coord2 %>% apply(1, test_coord, m=m)
  coord2[ok,]
}


# Part 1 --------------------------------------------------------------------------------------

# m0 = get_matrix(example=TRUE)
m0 = get_matrix(example=FALSE)

# ggmatrix(m0) + 
#   scale_fill_steps(breaks=0:9, limits=c(-1,9))



start_points = which(m0==0, arr.ind=TRUE)
start_points = start_points[order(start_points[,2]),][order(start_points[,1]),]

# .x = 5

scores = seq(nrow(start_points)) %>% 
  map_dbl(.progress=TRUE, ~{
    
    coord0 = start_points[.x,] %>% as_matrix() 
    coords = list(coord0)
    for(n in 2:10){
      coords[[n]] = apply(coords[[n-1]], MARGIN=1, 
                          FUN=next_neighbours, m=m0, simplify=FALSE) %>% 
        reduce(rbind) %>% 
        as_matrix()
    }
    score = nrow(unique(coords[[10]]))
    # browser()
    score
  })
print(scores)
sum(scores) %>% print #811


# Part 2 --------------------------------------------------------------------------------------
#part2


m2 = get_matrix(example=F)

start_points = which(m2==0, arr.ind=TRUE)
start_points = start_points[order(start_points[,2]),][order(start_points[,1]),]

scores2 = seq(nrow(start_points)) %>% 
  map_dbl(.progress=TRUE, ~{
    
    coord0 = start_points[.x,] %>% as_matrix() 
    coords = list(coord0)
    for(n in 2:10){
      coords[[n]] = apply(coords[[n-1]], MARGIN=1, 
                          FUN=next_neighbours, m=m2, simplify=FALSE) %>% 
        reduce(rbind) %>% 
        as_matrix()
    }
    score = nrow(coords[[10]])
    # browser()
    score
  })
# print(scores2)
sum(scores2) %>% print #1794

