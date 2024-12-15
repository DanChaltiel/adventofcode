
library(tidyverse)
Sys.setenv(LANGUAGE="en")
globalCallingHandlers(NULL)
rlang::global_entrace()

source("aoc2024/utils.R")


# Functions -----------------------------------------------------------------------------------

row_in_matrix = function(row, mm){
  apply(row, 1, function(x) {
    any(x["row"]==mm[,"row"] & x["col"]==mm[,"col"])
  })
}


# Part 1 --------------------------------------------------------------------------------------


input = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"
# input = "aoc2024/day12_input.txt" # 140x140


m = read_matrix(input, f=identity)
m2 = matrix(nrow=nrow(m), ncol=ncol(m))
# ggmatrix(m) %>% print()
plants = unique(as.character(m))

p = plants[1]

coord = which(m==p, arr.ind=TRUE)


coord0 = rbind(c(row=1, col=1)) #gen 0 = starting point

val_start = m[coord0]

voisins = neighbours(coord0, m)
ok1 = m[voisins] == val_start
ok2 = !row_in_matrix(voisins, coord0)

next_v = voisins[ok1&ok2,] #gen 1 = voisins R

next_v2 = next_v %>% 
  apply(1, neighbours, m=m, simplify=FALSE) %>% 
  map(~{
    # browser()
    ok1 = m[.x] == val_start
    ok2 = !row_in_matrix(.x, next_v)
    .x[ok1 & ok2,]
  }) #gen 2 = voisins des voisins R

#TODO boucle while pour gérer les listes
#TODO neighbours(diag=TRUE?)

#TODO rbind
# list(coord0, next_v, next_v2) %>% reduce(rbind) 


# i=j=5
# for(i in seq(nrow(m))){
#   for(j in seq(ncol(m))){
#     val = m[i,j]
#     # cli::cli_inform("Row {i}, col {j} -> val {val}")
#   }
# }



neighbours(coord[1,], m)
