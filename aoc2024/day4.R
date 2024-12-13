
library(tidyverse)


# Data ----------------------------------------------------------------------------------------


data_raw = read_lines("aoc2024/day4_input.txt") #140x140
data_raw = read_lines(
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")


m = data_raw %>% 
  map(~str_split_1(.x, pattern="") %>% matrix(nrow=1)) %>% 
  do.call(rbind, .)
print(flextable::flextable(as.data.frame(m)))


data = data_raw %>% 
  map(~str_split_1(.x, pattern="")) %>% 
  unlist() %>% 
  as_tibble_col() %>% 
  mutate(x=rep(seq_along(data_raw), each=length(data_raw)), 
         y=rep(seq_along(data_raw), times=length(data_raw)), 
         .before=1)

# Part 1  -------------------------------------------------------------------------------------



get_value = function(row, col){
  # print(c(row, col))
  a = data %>% filter(x==row, y==col) 
  if(nrow(a)==0) return(NA)
  a$value
}

test = function(row, col){
  a = c(-1, 0, 1)
  if(col==1) {
    value = get_value(row, col)
    cli::cli_inform("Testing {row}, {col}, {value}")
  }
  candidates = expand.grid(x=row+a, y=col+a) %>% 
    left_join(data, by=c("x", "y")) %>% 
    filter(value=="M")
  if(nrow(candidates)==0) return(0)
  candidates %>% 
    mutate(di=x-row, dj=y-col) %>% #direction x and y
    rowwise() %>% 
    mutate(
      # coord2 = paste(x+di, y+dj),
      value2 = get_value(x+di, y+dj),
      value3 = get_value(x+2*di, y+2*dj)
    ) %>% 
    filter(value2=="A" & value3=="S") %>% 
    nrow()
}


rslt = data %>% 
  filter(value=="X") %>% 
  rowwise() %>% 
  mutate(xmas = test(x, y))
rslt$xmas %>% sum #2718


# Part 2 --------------------------------------------------------------------------------------

test2 = function(row, col){
  a = c(-1, 1)
  # if(col>=1) {
  #   value = get_value(row, col)
  #   cli::cli_inform("Testing row={row}, col={col}, {value}")
  # }
  
  candidates = expand.grid(x=row+a, y=col+a) %>% 
    left_join(data, by=c("x", "y")) %>% 
    filter(value=="M" | value=="S") %>%
    mutate(di=x-row, dj=y-col,
           diag = sign(di)==sign(dj))
  if(nrow(candidates)<4) return(0)
  rtn = candidates %>% 
    split(.$diag) %>% 
    map_lgl(~length(unique(.x$value))==2) %>% 
    all()
  # print(rtn)
  
  
  # browser()
  # a[a$di==-1, ]
  # 
  # candidates %>% 
  #   mutate(di=x-row, dj=y-col) %>% #direction x and y
  #   rowwise() %>% 
  #   mutate(
  #     # coord2 = paste(x+di, y+dj),
  #     value2 = get_value(x+di, y+dj),
  #     value3 = get_value(x+2*di, y+2*dj)
  #   ) %>% 
  #   filter(value2=="A" & value3=="S") %>% 
  #   nrow()
  rtn
}


rslt2 = data %>% 
  filter(x>1, y>1, x<max(x), y<max(y)) %>% 
  filter(value=="A") %>% 
  rowwise() %>% 
  mutate(xmas = test2(x, y))

rslt2$xmas %>% sum #2046
