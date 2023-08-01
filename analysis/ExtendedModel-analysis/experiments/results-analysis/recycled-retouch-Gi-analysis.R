library(tidyverse)
library(ggthemes)
library(jtools)
library(ggpubr)
library(raster)
library(sp)
library(rgdal)
library(tmap)
library(spdep)
library(pscl)
library(Dict)
library(MASS)
library(QuantPsyc)

theme_set(theme_bw())

#count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")
#count.data = read_csv("/scratch/ec3307/recycling-Java/results/all-recycled-object-counts.csv")

count.data = read_csv("~/eclipse-workspace/recycling-Java/results/object-counts-with-skew.csv")


parameters = colnames(count.data[,8:19])
param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
#param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = param_list[,c(6:17)]
colnames(param_list) = parameters


#### retouched and recycled overlap ####
output = param_list[1,c(parameters)]
output$run = ""
output$high_recycled = 0
output$high_ret.prop = 0
output$rcycl.ret.overlap  = 0
output = output[0,]

for(i in 1:nrow(param_list)) {
  exp = count.data[which(
    count.data$max_use_intensity == c(param_list[i,parameters[1]]) &
      count.data$max_artifact_carry == c(param_list[i,parameters[2]]) &
      count.data$max_flake_size == c(param_list[i,parameters[3]]) &
      count.data$max_nodules_size == c(param_list[i,parameters[4]]) &
      count.data$blank_prob == c(param_list[i,parameters[5]]) &
      count.data$scavenge_prob == c(param_list[i,parameters[6]]) &
      count.data$overlap == c(param_list[i,parameters[7]]) &
      count.data$mu == c(param_list[i,parameters[8]]) &
      count.data$size_preference == c(param_list[i,parameters[9]]) &
      count.data$flake_preference == c(param_list[i,parameters[10]]) &
      count.data$min_suitable_flake_size == c(param_list[i,parameters[11]]) &
      count.data$strict_selection == c(param_list[i,parameters[12]])),]
  
  exp.end = exp[which(exp$time == "end"),]
  
  for(run in unique(exp.end$run)) {
    exp.run = exp.end[which(exp.end$run == run),]
    
    if(nrow(exp.run) > 0) {
      
      exp.g = exp.run %>% group_by(row, col) %>%
        summarize(count_obj = sum(total_count), 
                  count_rcycl = sum(count_recycled),
                  count_ret = sum(count_retouched), 
                  recycled_prop = count_rcycl/sum(total_count),
                  retouched_prop = count_ret/sum(total_count))
      
      exp.spat = exp.g
      coordinates(exp.spat) = ~col+row
      gridded(exp.spat) = TRUE
      exp.spat = as(exp.spat, "SpatialPolygonsDataFrame")
      exp.spat$row = exp.g$row
      exp.spat$col = exp.g$col
      
      nb = poly2nb(exp.spat, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      
      exp.spat$G.rcycl.prop = localG_perm(exp.spat$recycled_prop, lw, nsim = 100, zero.policy = T)
      exp.spat$G.retouch.prop = localG_perm(exp.spat$retouched_prop, lw, nsim = 100, zero.policy = T)
      
      exp.df = as.data.frame(exp.spat) %>%
        mutate(param_list[i, ],
               run = run)
      exp.df2 = exp.df[,c(6:22)]
      
      #readr::write_csv(exp.df, paste0("/scratch/ec3307/recycling-Java/output/artifact-data/output/exp", i, "_", run, "_rr-local-G.csv"), num_threads=1)
      
      recycled.rast = rasterFromXYZ(exp.df2[,c(1,2,3)])
      recycled.rast[recycled.rast < 2] = NA
      retouched.rast = rasterFromXYZ(exp.df2[,c(1,2,4)])
      retouched.rast[retouched.rast < 2] = NA

      r = overlay(recycled.rast, retouched.rast, fun=sum)
      rcycl.ret.o = 100 - freq(r, value = NA)

      output[nrow(output) + 1, ] =
        c(param_list[i,],
          run,
          100 - freq(recycled.rast, value = NA),
          100 - freq(retouched.rast, value = NA),
          rcycl.ret.o
        )
    }
  }
}
write.csv(output, file = "recycled-retouched-overlap.csv")

#### overlap analysis ####
# odata = read_csv("~/eclipse-workspace/recycling-Java/results/recycled-retouched-overlap.csv")
# 
# mean(odata$rcycl.ret.overlap)
# summary(odata$rcycl.ret.overlap)
# hist(odata$rcycl.ret.overlap)
# 
# rro = zeroinfl(rcycl.ret.overlap ~ ., odata[c(2:4, 6:13,17)])
# summary(rro)
# est = coef(rro, "zero")
# se = sqrt(diag(vcov(rro, "zero")))
# zrro.df = as.data.frame(cbind(est, se))
# zrro.df$name = "retouched and recycled object counts"
# zrro.df$var = rownames(zrro.df)
# rownames(zrro.df) = NULL
# zrro.df$model = "zero"
# 
# est = coef(rro, "count")
# se = sqrt(diag(vcov(rro, "count")))
# crro.df = as.data.frame(cbind(est, se))
# crro.df$name = "retouched and recycled object counts"
# crro.df$var = rownames(crro.df)
# rownames(crro.df) = NULL
# crro.df$model = "count"
# 
# df = rbind(zrro.df, crro.df)
# df = df  %>%
#   mutate(lower = est - se,
#          upper = est + se,
#          signif = ifelse(lower < 0 & upper > 0, FALSE, TRUE))
# 
# terms_dict = dict(
#   "overlap" = "technology overlap scenario",
#   "mu" = "mu",
#   "strict_selectionTRUE" = "strict selection: TRUE",
#   "size_preferenceTRUE" = "size preference: TRUE",
#   "flake_preferenceTRUE" = "flake preference: TRUE",
#   "scavenge_prob" = "scavenging probability",
#   "min_suitable_flake_size" = "min. selectable flake size",
#   "max_use_intensity" = "max. use intensity",
#   "max_flake_size" = "max. flake size",
#   "max_artifact_carry" = "max. artifact carry",
#   "blank_prob" = "blank probability",
#   "(Intercept)" = "(intercept)",
#   "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size",
#   .class = "character",
#   .overwrite = FALSE
# )
# term_levels = c(
#   "(intercept)",
#   "technology overlap scenario",
#   "mu",
#   "blank probability",
#   "scavenging probability",
#   "max. artifact carry",
#   "max. use intensity",
#   "max. flake size",
#   "min. selectable flake size",
#   "flake preference: TRUE",
#   "size preference: TRUE",
#   "strict selection: TRUE",
#   "max. flake size:min. selectable flake size"
# )
# 
# for(i in 1:nrow(df)) {
#   if(!is.na(df$var[i])) {
#     df$var_clean[i] = terms_dict[df$var[i]]
#     df$var_clean[i] = terms_dict[df$var[i]]
#   }
# }
# df$var_clean = factor(df$var_clean, levels = term_levels)
# df$var_clean = factor(df$var_clean, levels = term_levels)
# 
# df$model = factor(df$model, levels = c("zero", "count"))
# 
# 
# p = ggplot(df %>% filter(signif == T)) +
#   #geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
#   #geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = model, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
#   geom_col(aes(x = var_clean, y = est, fill = model, group = model)) +
#   geom_errorbar(aes(x = var_clean, y = est, ymax = upper, ymin = lower, group = model), width = 0.2, linewidth = 0.25) +
#   geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Dark2") +
#   facet_grid(name~model) +
#   labs(x = "parameter", y = "estimate") +
#   theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6),
#         axis.title = element_text(size = 7),
#         legend.position = "none")
# plot(p)
# ggsave(filename = "../figures/recycled-retouched-overlap.tiff", p,
#        dpi = 300, width = 7, height = 3.5)
# 
# 
# hist(odata$high_recycled)
# hist(odata$high_retouched)
# 
# fit2 = zeroinfl(rcycl.ret.overlap ~ ., odata[c(2:4, 6:13,15:17)])
# summary(fit2)
# est = coef(fit2, "zero")
# se = sqrt(diag(vcov(fit2, "zero")))
# zfit2.df = as.data.frame(cbind(est, se))
# zfit2.df$name = "retouched and recycled object counts"
# zfit2.df$var = rownames(zfit2.df)
# rownames(zfit2.df) = NULL
# zfit2.df$model = "zero"
# 
# est = coef(fit2, "count")
# se = sqrt(diag(vcov(fit2, "count")))
# cfit2.df = as.data.frame(cbind(est, se))
# cfit2.df$name = "retouched and recycled object counts"
# cfit2.df$var = rownames(cfit2.df)
# rownames(cfit2.df) = NULL
# cfit2.df$model = "count"
# 
# df = rbind(zfit2.df, cfit2.df)
# df = df  %>%
#   mutate(lower = est - se,
#          upper = est + se) %>%
#   rowwise() %>%
#   mutate(signif = !between(0, lower, upper))
# 
# terms_dict = dict(
#   "overlap" = "technology overlap scenario",
#   "mu" = "mu",
#   "strict_selectionTRUE" = "strict selection: TRUE",
#   "size_preferenceTRUE" = "size preference: TRUE",
#   "flake_preferenceTRUE" = "flake preference: TRUE",
#   "scavenge_prob" = "scavenging probability",
#   "min_suitable_flake_size" = "min. selectable flake size",
#   "max_use_intensity" = "max. use intensity",
#   "max_flake_size" = "max. flake size",
#   "max_artifact_carry" = "max. artifact carry",
#   "blank_prob" = "blank probability",
#   "(Intercept)" = "(intercept)",
#   "max_flake_size:min_suitable_flake_size" = "max. flake size:min. selectable flake size",
#   "high_recycled" = "high recycled object count squares", 
#   "high_retouched" = "high retouched object count squares",
#   .class = "character",
#   .overwrite = FALSE
# )
# term_levels = c(
#   "(intercept)",
#   "technology overlap scenario",
#   "mu",
#   "blank probability",
#   "scavenging probability",
#   "max. artifact carry",
#   "max. use intensity",
#   "max. flake size",
#   "min. selectable flake size",
#   "flake preference: TRUE",
#   "size preference: TRUE",
#   "strict selection: TRUE",
#   "max. flake size:min. selectable flake size", 
#   "high recycled object count squares",
#   "high retouched object count squares"
# )
# 
# for(i in 1:nrow(df)) {
#   if(!is.na(df$var[i])) {
#     df$var_clean[i] = terms_dict[df$var[i]]
#     df$var_clean[i] = terms_dict[df$var[i]]
#   }
# }
# df$var_clean = factor(df$var_clean, levels = term_levels)
# 
# df$model = factor(df$model, levels = c("zero", "count"))
# 
# high = df %>% filter(str_detect(var, "high"))
# 
# p2 = ggplot(high %>% filter(signif == T)) +
#   #geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
#   #geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = model, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
#   geom_col(aes(x = var_clean, y = est, fill = model, group = model)) +
#   geom_errorbar(aes(x = var_clean, y = est, ymax = upper, ymin = lower, group = model), width = 0.2, linewidth = 0.25) +
#   geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Dark2") +
#   facet_grid(name~model) +
#   labs(x = "parameter", y = "estimate") +
#   theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6),
#         axis.title = element_text(size = 7),
#         legend.position = "none")
# plot(p2)
# ggsave(filename = "../figures/supplementary-figures/recycled-retouched-overlap_high-squares.tiff", p2,
#        dpi = 300, width = 7, height = 3.5)
# 
# 
# 
# hist(odata$high_retouched)
# fit3 = zeroinfl(high_retouched ~ ., odata[c(2:4,6:13,16)])
# summary(fit3)
