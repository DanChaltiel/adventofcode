
read_matrix = function(input, f=as.numeric){
  read_lines(input) %>% 
    str_split("") %>% 
    map(~matrix(f(.x), nrow=1)) %>% 
    reduce(rbind)
}

as_matrix = function(x){
  if(!is.matrix(x)) x = rbind(x)
  x
}

row_in_matrix = function(row, mm){
  apply(row, 1, function(x) {
    any(x["row"]==mm[,"row"] & x["col"]==mm[,"col"])
  })
}

exclude_rows = function(m, rows){
  m[!row_in_matrix(m, rows), ]
}

neighbours = function(coord, m, diag=FALSE){
  coord = as_matrix(coord)
  stopifnot(ncol(coord)==2)
  colnames(coord) = c("row", "col")
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
  coord2 = coord + d
  ok = coord2 %>% apply(1, test_coord, m=m)
  coord2[ok,] %>% exclude_rows(coord)
}

#coord est valide pour la matrice m
test_coord = function(coord, m){
  coord = as_matrix(coord)
  test_inf = coord[,"row"]>0 & coord[,"col"]>0
  test_sup = coord[,"row"]<=nrow(m) & coord[,"col"]<=ncol(m)
  test_inf & test_sup
}


ggmatrix = function(m, fun=NULL){
  df = m %>%
    as_tibble() %>% 
    suppressMessages() %>% 
    rownames_to_column("row") %>%
    pivot_longer(-row, names_to = "col", values_to="value") %>% 
    mutate(col=parse_number(col), 
           col=factor(col), row=fct_rev(factor(as.numeric(row))))
  
  if(!is.null(fun)){
    fun = rlang::as_function(fun)
    df = df %>% 
      mutate(value=fun(value))
  }
  df %>% 
    ggplot(aes(y=row, x=col, fill=value, label=value)) +
    geom_tile(na.rm=TRUE) + 
    geom_text(na.rm=TRUE) +
    scale_x_discrete(position = "top") +
    # scale_fill(drop = FALSE) +
    coord_fixed()
  
}

ggmatrix2 = function(m, m_ref=m0){
  p1 = mat_plot(m0) + ggtitle("Reference")
  p2 = mat_plot(m) + ggtitle("Extended")
  
  pp = p0 | p2 & plot_layout(guides="collect")
  print(pp)
  invisible(NULL)
}
