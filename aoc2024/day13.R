input = 
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

# input = "aoc2024/day13_input.txt"

read_input = function(input){
  x = read_lines(input)
  sep = which(x=="") %>% c(length(x)+1)
  sep %>% 
    map2(lag(., default=0), ~x[(.y+1):(.x-1)]) %>% 
    map(~{
      .x %>% 
        paste(collapse=",") %>% 
        as_tibble_col("str") %>% 
        separate_wider_regex(str, 
                             patterns=c("Button A: X", a_x=".\\d+", ", Y", a_y=".\\d+", 
                                        ",Button B: X", b_x=".\\d+", ", Y", b_y=".\\d+", 
                                        ",Prize: X=", p_x=".\\d+", ", Y=", p_y=".\\d+")) %>% 
        mutate_all(as.numeric)
    }) %>% 
    list_rbind()
}


x = read_input(input)
# x_bak = x
# x = x_bak

# Part 1 --------------------------------------------------------------------------------------


a = x %>% 
  mutate(
    nb = (a_y*p_x - p_y*a_x)/(a_y*b_x - a_x*b_y),
    na = (p_x - nb*b_x)/a_x,
    ok = na%%1==0 & nb%%1==0,
    price = ifelse(ok, 3*na + nb, NA)
  )

a %>% 
  filter(na<=100 & nb<=100) %>% 
  filter(ok) %>% 
  pull(price) %>% 
  sum()#40069 ok
 

# Part 2 --------------------------------------------------------------------------------------
 
S = 10000000000000

a = x %>% 
  mutate(
    p_y = S+p_y,
    p_x = S+p_x,
    nb = (a_y*p_x - p_y*a_x)/(a_y*b_x - a_x*b_y),
    na = (p_x - nb*b_x)/a_x,
    ok = na%%1==0 & nb%%1==0,
    price = ifelse(ok, 3*na + nb, NA)
  )

a %>% 
  # filter(na<=100 & nb<=100) %>%
  filter(ok) %>% 
  pull(price) %>% 
  sum() #71493195288102 ok
