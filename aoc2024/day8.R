library(tidyverse)
library(patchwork)

b=browser

test_i = function(mat_i, mat_ref){
  stopifnot(ncol(mat_i)==2)
  test_inf = mat_i[,1]>0 & mat_i[,2]>0
  test_sup = mat_i[,1]<=nrow(mat_ref) & mat_i[,2]<=ncol(mat_ref)
  test_inf & test_sup
}

# Functions -----------------------------------------------------------------------------------


permute = function(operators, n_spaces){
  p = gtools::permutations(length(operators), r=n_spaces, v=operators, 
                           set=FALSE, repeats.allowed=TRUE)
  lapply(seq_len(nrow(p)), function(i) p[i,])
}

find_antinodes = function(a, b){
  d = a-b
  # browser()
  rbind(a+d, b-d)
}

correct_i = function(i, mat=m0){
  stopifnot(length(i)==2)
  # b()
  # if(any(i<0)) b()
  # if(all(i==structure(14:13, names = c("row", "col")))) browser()
  x = try(mat[i, exact=TRUE], silent=TRUE)
  if(any(i<0) || inherits(x, "try-error") || length(x)<2){
    return(c(NA, NA))
  }
  i
}


# Data ----------------------------------------------------------------------------------------


input = 
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"
input = "aoc2024/day8_input.txt"

x = read_lines(input)

m0 = x %>% 
  str_split("") %>% 
  map(~matrix(.x, nrow=1)) %>% 
  reduce(rbind)

# Part 1 --------------------------------------------------------------------------------------

if(FALSE){
  
  m1 = m0
  antennas = unique(as.character(m0)) %>% setdiff(".")
  
  
  for(antenna in antennas){
    pos = which(m0==antenna, arr.ind = TRUE)
    row_combinations = combn(1:nrow(pos), 2, simplify=FALSE)  
    
    a = map(row_combinations, ~{
      find_antinodes(pos[.x[1],], pos[.x[2],]) %>% 
        apply(MARGIN=1, FUN=correct_i, mat=m0, simplify=FALSE) %>% 
        keep(~!any(is.na(.x))) %>%
        reduce(rbind, .init=NULL)
    }) %>% 
      reduce(rbind)
    # browser()
    m1[a] = "#"
  }
  
  sum(m1=="#") %>% print() #351
  
}

# Part 2 --------------------------------------------------------------------------------------


m2 = m0
antennas = unique(as.character(m0)) %>% setdiff(".")

# mat_plot(m0) %>% print()


find_antinodes2 = function(a, b, mat){
  d = a-b
  n_max = max(nrow(mat)/2-1, ncol(mat)/2-1)
  n_max = max(nrow(mat), ncol(mat))
  l = matrix(nrow=n_max*2, ncol=2)
  for(i in seq(n_max)){
    if(i>1) l[i,] = a - i*d
    l[i+n_max,] = a + i*d
  }
  ok = test_i(l, mat)
  # browser()
  l[ok,]
}

# antenna = antennas[1]
# .x = row_combinations[[1]]
for(antenna in antennas){
  pos = which(m0==antenna, arr.ind = TRUE)
  row_combinations = combn(1:nrow(pos), 2, simplify=FALSE)  
  
  a = map(row_combinations, ~{
    find_antinodes2(pos[.x[1],], pos[.x[2],], m0)
  }) %>% 
    reduce(rbind)
  m2[a] = "#"
}

# mat_plot(m2)
# plot2(m2)
sum(m2=="#") %>% print() #1136 too low
sum(m2!=".") %>% print() #1259

# Utils ---------------------------------------------------------------------------------------

mat_plot = function(m){
  m %>%
    as_tibble() %>% 
    suppressMessages() %>% 
    rownames_to_column("row") %>%
    pivot_longer(-row, names_to = "col", values_to="value") %>% 
    mutate(col=parse_number(col), value=factor(value, levels=c("#", ".", antennas)),
           col=factor(col), row=fct_rev(factor(as.numeric(row)))) %>% 
    ggplot(aes(y=row, x=col, fill=value, label=value)) +
    geom_tile(na.rm=TRUE) + 
    geom_text(na.rm=TRUE) +
    scale_x_discrete(position = "top") +
    scale_fill_discrete(drop = FALSE) +
    coord_fixed()
  
}

p0 = mat_plot(m0) + ggtitle("Baseline")
p1 = mat_plot(m1) + ggtitle("Part 1")
p2 = mat_plot(m2) + ggtitle("Part 2")

# pp = p0 | p1 & plot_layout(guides="collect")
pp = p0 | p2 & plot_layout(guides="collect")
print(pp)

plot2 = function(m){
  p1 = mat_plot(m0) + ggtitle("Baseline")
  p2 = mat_plot(m) + ggtitle("Extended")
  
  # pp = p0 | p1 & plot_layout(guides="collect")
  pp = p0 | p2 & plot_layout(guides="collect")
  print(pp)
  invisible(NULL)
}

