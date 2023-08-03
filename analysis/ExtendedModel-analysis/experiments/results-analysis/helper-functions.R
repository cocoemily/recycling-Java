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


####currently unused####
plotREheatmap = function(data) {
  data$groupID = as.numeric(data$groupID)
  data = data %>% left_join(row.col, by = c("groupID" = "square"))
  
  data[, "sd"] <- data[, "sd"] * qnorm(1-((1-0.95)/2))
  data[, "ymax"] <- data[, "median"] + data[, "sd"]
  data[, "ymin"] <- data[, "median"] - data[, "sd"]
  data[, "sig"] <- data[, "ymin"] > 0 | data[, "ymax"] < 0
  hlineInt <- 0
  data$sig.lab = ifelse(data$sig, "", "X")
  
  p2 = ggplot(data, aes(x = as.factor(col), y = as.factor(row))) +
    geom_tile(aes(fill = median, alpha = sig), color = "black") +
    geom_text(aes(label = sig.lab), color = "red") +
    coord_fixed() +
    scale_fill_gradient2(low = "#075AFF",
                         mid = "#FFFFCC",
                         high = "#FF0000", 
                         limits = c(-0.15, 0.15)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.title = element_text(size = 6)) +
    labs(x = NULL, y = NULL, fill = "random effect", alpha = "signf.") +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top") +
    guides(alpha = "none")
  #plot(p2)
  return(p2)
  
}
