
library(tidyverse)
rlang::global_entrace()


# Functions -----------------------------------------------------------------------------------

get_disk_map = function(x){
  
  a = x %>% str_split_1("") %>% as.numeric()
  a_even = seq_along(a)%%2==0
  id_files = a[!a_even]
  id_frees = a[a_even]
  
  y1 = id_files %>% 
    imap(~{
      rep(.y-1, .x)
    })
  
  y2 = id_frees %>% 
    map(~{
      rep(".", .x)
    }) 
  
  length(y1) = max(length(y1), length(y2))
  length(y2) = max(length(y1), length(y2))
  
  unlist(c(rbind(y1, y2)))
}

sort_disk_map = function(disk_map){
  # browser()
  n_dots = sum(disk_map==".")
  l = length(disk_map)
  
  disk_map[disk_map=="."] = rev(disk_map[disk_map!="."] %>% tail(n_dots))
  disk_map[(l-n_dots+1):l] = "."
  
  disk_map
}

checksum = function(x){
  a = as.numeric(x[x!="."])
  sum(a*seq(0, length(a)-1))
}


# Part 1 --------------------------------------------------------------------------------------

if(FALSE){
  
  input = "2333133121414131402"
  # input = "23331331214141314022222222"
  # input = read_lines("aoc2024/day9_input.txt")
  
  disk_map = get_disk_map(input)
  # paste(disk_map, collapse="")=="00...111...2...333.44.5555.6666.777.888899"
  
  disk_map_sorted = sort_disk_map(disk_map)
  # paste(disk_map_sorted, collapse="")=="0099811188827773336446555566.............."
  
  # print(disk_map)
  checksum(disk_map_sorted) %>% format(scientific=FALSE) %>% print() #OK !
  
  
}


# Part 2 --------------------------------------------------------------------------------------
#this code is largely overcomplicated...
seq2 = function(x){
  stopifnot(length(x)==2)
  seq(x[1], x[2])
}

insert_file = function(disk, space, file){
  if(length(space) < length(file)) return(disk)
  disk[space][seq_along(file)] = disk[file]
  disk[file] = "."
  disk
}

sort_disk_map2_bak = function(disk_map, cur_file_index=NULL){
  
  disk_run = rle(disk_map)
  
  i = disk_run$lengths %>% cumsum()
  i = cbind(start=lag(i, default=0)+1, stop=i) #indices des runs
  
  is_space = disk_run$values=="."
  is_file = !is_space
  length_space = disk_run$lengths[is_space]
  length_files = disk_run$lengths[is_file]
  
  if(is.null(cur_file_index)){
    cur_file_index = length(length_files)
  }
  
  cur_file_real_index = which(is_file)[cur_file_index]
  
  space_ok = length_space %>% 
    map_lgl(~{
      length_files[cur_file_index]<=.x
    })

  space_to_fill = min(which(space_ok))
  space_to_fill_index = which(is_space)[space_to_fill]
  
  disk_map[seq2(i[cur_file_real_index,])]
  disk_map[seq2(i[space_to_fill_index,])]
  
  
  disk_map %>% 
    insert_file(space=seq2(i[space_to_fill_index,]), 
                file=seq2(i[cur_file_real_index,]))
}

sort_disk_map2 = function(disk_map, cur_value){
  
  disk_run = rle(disk_map)
  i = disk_run$lengths %>% cumsum()
  i = cbind(start=lag(i, default=0)+1, stop=i) #indices des runs
  
  is_space = disk_run$values=="."
  is_file = !is_space
  length_space = disk_run$lengths[is_space]
  length_files = disk_run$lengths[is_file]
  
  cur_file_index = which(disk_run$values[is_file]==cur_value)
  cur_file_real_index = which(disk_run$values==cur_value)
  first_dot_index = min(which(disk_run$values=="."))
  if(cur_file_real_index<first_dot_index) return(disk_map) #only move files backward
  
  space_ok = length_space %>% 
    map_lgl(~{
      length_files[cur_file_index]<=.x
    })
  
  if(!any(space_ok)) return(disk_map) #if no possible move, return
  space_to_fill = min(which(space_ok))
  space_to_fill_index = which(is_space)[space_to_fill]
  
  space_index = i[space_to_fill_index,]
  file_index = i[cur_file_real_index,]
  
  if(space_index["stop"] > file_index["start"]) return(disk_map)
  
  disk_map %>% 
    insert_file(space=seq2(space_index), 
                file=seq2(file_index))
}


# input = "2333133121414131402"
input = read_lines("aoc2024/day9_input.txt")

disk_map = get_disk_map(input)
disk_map2 = disk_map

max_val = max(as.numeric(disk_map[disk_map!="."]))
for(i in rev(seq(max_val))){
  if(i%%100==0) print(i)
  disk_map2 = sort_disk_map2(disk_map2, cur_value=i)
}

if(paste(disk_map2, collapse="") == "00992111777.44.333....5555.6666.....8888.."){
  cli::cli_alert_success("Youpi")
}


checksum2 = function(x){
  a = as.numeric(na_if(x, "."))
  sum(a*seq(0, length(a)-1), na.rm=TRUE)
}

checksum2(disk_map2) %>% format(scientific=FALSE) %>% print() #6327174563252 !

beepr::beep()
