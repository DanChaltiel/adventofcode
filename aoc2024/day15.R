example1="########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"

example2 = "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

example3 = "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"

input = "aoc2024/day15_input.txt"


# Function ------------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)

read_input = function(input){
  x = read_lines(input)
  sep = which(x=="")
  input2 = x[seq(1, sep-1)] %>% paste(collapse="\n")
  m = read_matrix(input2, f=identity)
  mv = x[seq(sep+1, length(x))] %>% paste(collapse="") %>% str_split_1("")
  lst(m,mv)
}

point = function(...){
  x = c(...)
  stopifnot(length(x)==2)
  rbind(c(row=x[1], col=x[2]))
}

next_pos = function(pos, dir){
  stopifnot(dir %in% c("^", ">", "v", "<"))
  mvt=move_coord[[dir]]
  pos+mvt[rep(1,nrow(pos)),]
}

move_coord = list("^"=point(-1,0), "v"=point(+1,0), 
                  "<"=point(0,-1), ">"=point(0,+1))

wander = function(moves, debug=FALSE){
  for(i in moves){
    move(i)
    if(debug){
      cli::cli_inform("Moving by {.val {i}}")
      ggmatrix(m) %>% print()
      browser()
    }
  }
}


# Part 1 --------------------------------------------------------------------------------------

if(FALSE){
  
  # x_bak = read_input("aoc2024/day15_input.txt")
  # table(x_bak$mv)
  # table(x_bak$m)
  
  
  move = function(dir, what="@", pos="robot"){
    if(identical(pos, "robot")) pos = which(m=="@", arr.ind=TRUE)
    
    pos2 = next_pos(pos, dir)
    # if(what!="@") browser()
    
    if(m[pos2] == "#") return(0)
    
    if(m[pos2] == "."){
      m[pos2] <<- what
      m[pos] <<- "."
      return(1)
    }
    
    if(m[pos2] == "O"){
      r = move(dir, what="O", pos=pos2) 
      if(r==1) move(dir, what=what, pos=pos)
      return(r)
    }
    return(0)
  }
  # 
  # move("^")
  # move(">")
  # move(">")
  # move(">")
  # move("v")
  # ggmatrix(m) %>% print()
  # ggmatrix(m0) %>% print()
  
  distance = function(p){
    p = as_matrix(p)
    100*(p[,"row"]-1) + (p[,"col"]-1)
  }
  
  
  x = read_input(example2)
  m0 = m = x$m
  moves = x$mv
  
  m = x$m
  wander(x$mv)
  
  rslt = which(m=="O", arr.ind=TRUE) %>% 
    apply(1, distance) %>% 
    sum()
  
  print(rslt) #1398947 ok
  
  
  p = ggmatrix(m) + ggmatrix(m0) + plot_layout(guides="collect")
  print(p)
  
}

# Part 2 --------------------------------------------------------------------------------------


read_input2 = function(input){
  input = read_lines(input) %>% 
    str_replace_all("#", "##") %>% 
    str_replace_all("O", "\\[\\]") %>% 
    str_replace_all("\\.", "\\.\\.") %>% 
    str_replace_all("@", "@\\.") %>% 
    paste(collapse="\n")
  read_input(input)
}

is_box = function(pos){
  if(is.numeric(pos)) pos = m[pos]
  pos == "[" | pos == "]"
}

get_copain = function(pos){
  stopifnot(is_box(pos))
  d = if(m[pos] == "[") point(0,1) else point(0,-1)
  pos+d
}

move = function(dir, what="@", pos="robot", copain=FALSE){
  if(identical(pos, "robot")) pos = which(m=="@", arr.ind=TRUE)
  who = rbind(pos, next_boxes(pos, dir))
  target = next_pos(who, dir)
  # browser()
  if(all(can_move(who, dir))){
    # browser()
    who_val = m[who]
    m[who] <<- "."
    m[target] <<- who_val
    return(TRUE)
  }
  return(FALSE)
}
  
next_boxes = function(pos, dir){
  pos2 = next_pos(pos, dir)
  rtn = NULL
  for(i in seq(nrow(pos2))){
    r = as_matrix(pos2[i,])
    if(is_box(r)){
      # browser()
      r2 = get_copain(r)
      if(row_in_matrix(r2, pos)) next
      boxes = rbind(r, r2)
      n_boxes1 = next_boxes(r, dir)
      n_boxes2 = next_boxes(r2, dir)
      # n_boxes1 = 1
      # n_boxes2 = 2
      rtn = rbind(rtn, boxes, n_boxes1, n_boxes2)
    }
  }
  rtn
}
# next_boxes(point(3,10), dir=">")
# next_boxes(point(5,8), dir="^")

can_move = function(pos, dir){
  pos2 = next_pos(pos, dir)
  m[pos2] != "#"
}
# can_move(pos, dir)




x = read_input2(input)
m0 = m = x$m
ggmatrix(m) %>% print()


wander(x$mv, debug=F)


rslt = which(m=="[", arr.ind=TRUE) %>% 
  apply(1, distance) %>% 
  sum() #1397393 ok

# 
# m


# Plot ----------------------------------------------------------------------------------------

p1 = ggmatrix(m) + ggtitle("Stop")
p0 = ggmatrix(m0) + ggtitle("Start")
p = p0 + p1 + plot_layout(guides="collect") & theme(legend.position="bottom")
print(p)
# print(p & coord_fixed(1.8))


stop("OK")

