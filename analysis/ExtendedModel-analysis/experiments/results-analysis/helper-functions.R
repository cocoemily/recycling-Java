#helper functions

pairwiseKS = function(data, group_var, variable) {
  group.list = unique(data[group_var])
  groups = unlist(group.list)
  groups = as.numeric(sort(groups))
  
  results = data.frame(group1 = character(0), 
                       group2 = character(0), 
                       p.value = numeric(0))
  
  for(l in 1:length(groups)) {
    for(l2 in 1:length(groups)) {
      filter1 = paste0(group_var, " == ", groups[l])
      filter2 = paste0(group_var, " == ", groups[l2])
      
      test = ks.test(
        x = as.vector((data %>% filter(rlang::eval_tidy(rlang::parse_expr(filter1))))[variable])[[1]], 
        y = as.vector((data %>% filter(rlang::eval_tidy(rlang::parse_expr(filter2))))[variable])[[1]], 
        alternative = "two.sided",
        simulate.p.value = T
      )
      
      results[nrow(results) + 1, ] = c(l, l2, test$p.value)
    }
    
  }
  
  results2 = results %>% filter(group1 != group2) %>% 
    rowwise() %>%
    mutate(key = paste(sort(c(group1, group2)), collapse="")) %>%
    distinct(key, .keep_all=T) %>%
    select(-key) %>% 
    mutate(Comparison = paste0(group1, " : ", group2)) %>%
    select(Comparison, p.value)
  
  return(results2)
  
}
