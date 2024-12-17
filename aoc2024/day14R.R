input = 
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

# input = "p=2,4 v=2,-3
# p=6,3 v=-1,-3"
input = "aoc2024/day14_input.txt"

read_input = function(input){
  read_lines(input) %>% 
    as_tibble_col("str") %>% 
    separate_wider_regex(
      str, 
      patterns=c("p=", x="-?\\d+", ",", y="-?\\d+", " v=", vx="-?\\d+", ",", vy="-?\\d+")
    ) %>% 
    mutate_all(as.numeric)
}

get_data = function(t){
  read_input(input) %>% 
    mutate(x2 = (x+vx*t) %% box$x, 
           y2 = (y+vy*t) %% box$y)
}
get_data = memoise::memoise(get_data)


# Part 1 --------------------------------------------------------------------------------------


box = list(x=101, y=103)

data = get_data(t=100)

rtn = expand.grid(x=c("sup", "inf"), y=c("sup", "inf")) %>% 
  rowwise() %>% 
  mutate(n = {
    fx = if(x=="sup") `>` else `<`
    fy = if(y=="sup") `>` else `<`
    data %>% 
      filter(fx(x2, box$x/2 - 0.5) & fy(y2, box$y/2 - 0.5)) %>% 
      nrow()
  })

prod(rtn$n) %>% print() # 229980828 ok


# Part 2 --------------------------------------------------------------------------------------



stop("ok")


data_t = map(seq(1e4), get_data, .progress=TRUE)

i = data_t %>% 
  map_dbl(~var(.x$x2)*var(.x$y2))
rslt = which.min(i)
print(rslt) #7132 ok

data_t %>% 
  keep(~var(.x$x2)*var(.x$y2) == i[rslt]) %>% 
  map(~{
    p = .x %>% 
      ggplot() +
      aes(x=x2, y=y2) +
      geom_point() +
      scale_y_continuous(minor_breaks=FALSE, transform="reverse")+
      scale_x_continuous(minor_breaks=FALSE, position="top")
    print(p)
    browser()
  })


x = data_t[[100]]

symmetric = data_t %>% 
  keep(~mean(.x$x2[.x$x2!=50]>50) == 0.5)


# Plot ----------------------------------------------------------------------------------------

a = arrow(length = unit(0.3, "inches"))
ggplot(x) +
  aes(x=x, y=y, xend=x+vx, yend=y+vy) +
  geom_point() +
  geom_segment(arrow=a) +
  # scale_y_continuous(breaks=scales::breaks_width(1), minor_breaks=FALSE, transform="reverse")+
  scale_y_reverse()+
  scale_x_continuous(breaks=scales::breaks_width(1), minor_breaks=FALSE, position="top")

data %>% 
  ggplot() +
  aes(x=x2, y=y2) +
  geom_point()

data %>% 
  # filter(x2 != box$x/2 - 0.5) %>% 
  # filter(y2 != box$y/2 - 0.5) %>% 
  ggplot() +
  aes(x=x2, y=y2) +
  geom_point() +
  scale_y_continuous(minor_breaks=FALSE, transform="reverse")+
  scale_x_continuous(breaks=scales::breaks_width(1), minor_breaks=FALSE, position="top")
