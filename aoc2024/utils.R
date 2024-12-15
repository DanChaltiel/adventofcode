
read_matrix = function(input){
  read_lines(input) %>% 
    str_split("") %>% 
    map(~matrix(as.numeric(.x), nrow=1)) %>% 
    reduce(rbind)
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
