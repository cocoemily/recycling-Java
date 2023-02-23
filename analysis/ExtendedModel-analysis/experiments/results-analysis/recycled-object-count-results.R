##recycled object counts
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
#library(Dict)

theme_set(theme_bw())

#count.data = read_csv("~/eclipse-workspace/recycling-Java/results/all-recycled-object-counts.csv")
count.data = read_csv("/scratch/ec3307/recycling-Java/results/all-recycled-object-counts.csv")

parameters = colnames(count.data[,6:17])
#param_list = read_csv("~/eclipse-workspace/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = read_csv("/scratch/ec3307/recycling-Java/run-scripts/ExtendedModel-model-runs/parameters.csv")
param_list = param_list[,c(6:17)]
colnames(param_list) = parameters

#### recycled object counts ####
# run.avg = count.data %>%
#   group_by_at(c("obj_type", "time", parameters)) %>%
#   summarize(avg_recycled = mean(count_recycled))
# 
# flake.labs = c("flake preference", "nodule preference")
# names(flake.labs) = c("TRUE", "FALSE")
# size.labs = c("size preference", "no size preference")
# names(size.labs) = c("TRUE", "FALSE")
# strict.labs = c("strict selection", "no strict selection")
# names(strict.labs) = c("TRUE", "FALSE")
# 
# p1 = ggplot(run.avg %>% filter(overlap == 2)) +
#   #geom_boxplot(aes(x = time, y = avg_recycled, group = time), alpha = 0.25, color = "grey20") +
#   geom_boxplot(aes(x = time, y = avg_recycled, fill = obj_type)) +
#   facet_grid(flake_preference + size_preference ~ strict_selection ,
#              labeller = labeller(flake_preference = flake.labs,
#                                  size_preference = size.labs,
#                                  strict_selection = strict.labs)) +
#   scale_x_discrete(limits = rev) +
#   scale_fill_brewer(palette = "Paired") +
#   theme(legend.title = element_blank(),
#         strip.text = element_text(size = 8),
#         axis.text = element_text(size = 6)) +
#   labs(y = "number of recycled objects")
# plot(p1)
# 
# p2 = ggplot(run.avg %>% filter(overlap == 1)) +
#   geom_boxplot(aes(x = time, y = avg_recycled, fill = obj_type)) +
#   facet_grid(flake_preference + size_preference ~ strict_selection ,
#              labeller = labeller(flake_preference = flake.labs,
#                                  size_preference = size.labs,
#                                  strict_selection = strict.labs)) +
#   scale_x_discrete(limits = rev) +
#   scale_fill_brewer(palette = "Paired") +
#   theme(legend.title = element_blank(),
#         strip.text = element_text(size = 8),
#         axis.text = element_text(size = 6)) +
#   labs(y = "number of recycled objects")
# plot(p2)
# 
# ggsave(filename = "../figures/supplementary-figures/recycled-object-counts.tiff",
#        plot = ggarrange(p2, p1,
#                         common.legend = T, labels = "AUTO"),
#        dpi = 300, width = 10, height = 6)
# 
# 
# test.p1 = ggplot(run.avg %>% filter(overlap == 1), aes(x = time, y = avg_recycled, fill = obj_type, group =obj_type)) +
#   stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
#   stat_summary(geom = "errorbar", position = position_dodge(0.9),
#                width = 0.05,
#                # fun.data = "mean_cl_normal") +
#                fun.max = function(x) mean(x) +
#                  qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
#                fun.min = function(x) mean(x) -
#                  qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
#   facet_grid(flake_preference + size_preference ~ strict_selection ,
#              labeller = labeller(flake_preference = flake.labs,
#                                  size_preference = size.labs,
#                                  strict_selection = strict.labs)) +
#   scale_fill_brewer(palette = "Paired") +
#   scale_x_discrete(limits = rev) +
#   theme(legend.title = element_blank(),
#         strip.text = element_text(size = 8),
#         axis.text = element_text(size = 6)) +
#   labs(y = "average number of recycled objects")
# plot(test.p1)
# 
# test.p2 = ggplot(run.avg %>% filter(overlap == 2), aes(x = time, y = avg_recycled, fill = obj_type, group =obj_type)) +
#   stat_summary(fun = "mean", geom = "bar", position = position_dodge()) +
#   stat_summary(geom = "errorbar", position = position_dodge(0.9),
#                width = 0.05,
#                # fun.data = "mean_cl_normal") +
#                fun.max = function(x) mean(x) +
#                  qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
#                fun.min = function(x) mean(x) -
#                  qt(.975, df = length(x)) * sd(x) / sqrt(length(x))) +
#   facet_grid(flake_preference + size_preference ~ strict_selection ,
#              labeller = labeller(flake_preference = flake.labs,
#                                  size_preference = size.labs,
#                                  strict_selection = strict.labs)) +
#   scale_fill_brewer(palette = "Paired") +
#   scale_x_discrete(limits = rev) +
#   theme(legend.title = element_blank(),
#         strip.text = element_text(size = 8),
#         axis.text = element_text(size = 6)) +
#   labs(y = "average number of recycled objects")
# plot(test.p2)
# 
# ggsave(filename = "../figures/supplementary-figures/run-averaged_recycled-object-counts.tiff",
#        plot = ggarrange(test.p1, test.p2,
#                         common.legend = T, labels = "AUTO"),
#        dpi = 300, width = 10, height = 6)
# 
# ggsave(filename = "../figures/supplementary-figures/all-recycled-object-count-data.tiff",
#        plot = ggarrange(p2, p1, test.p1, test.p2,
#                         common.legend = T, labels = "AUTO", ncol = 2, nrow = 2),
#        dpi = 300, width = 10, height = 10)


#### retouched and recycled overlap ####
output = count.data[,c(parameters, "run")]
output$high_recycled = 0
output$high_retouched = 0
output$rcycl.ret.overlap  = 0
output = output[0,]

for(i in 1:nrow(param_list)) {
  exp = count.data %>%
    filter(max_use_intensity == c(param_list[i,parameters[1]]) &
             max_artifact_carry == c(param_list[i,parameters[2]]) &
             max_flake_size == c(param_list[i,parameters[3]]) &
             max_nodules_size == c(param_list[i,parameters[4]]) &
             blank_prob == c(param_list[i,parameters[5]]) &
             scavenge_prob == c(param_list[i,parameters[6]]) &
             overlap == c(param_list[i,parameters[7]]) &
             mu == c(param_list[i,parameters[8]]) &
             size_preference == c(param_list[i,parameters[9]]) &
             flake_preference == c(param_list[i,parameters[10]]) &
             min_suitable_flake_size == c(param_list[i,parameters[11]]) &
             strict_selection == c(param_list[i,parameters[12]]))

  exp.end = exp[which(exp$time == "end"),]

  for(run in c("run1", "run2", "run3", "run4", "run5")) {
    exp.run = exp.end[which(exp.end$run == run),]

    if(nrow(exp.run) > 0) {

      exp.g = exp.run %>% group_by(row, col) %>%
        summarize(count_recycled = sum(count_recycled),
                  count_retouched = sum(count_retouched))

      exp.spat = exp.g
      coordinates(exp.spat) = ~col+row
      gridded(exp.spat) = TRUE
      exp.spat = as(exp.spat, "SpatialPolygonsDataFrame")
      exp.spat$row = exp.g$row
      exp.spat$col = exp.g$col

      nb = poly2nb(exp.spat, queen = T)
      lw = nb2listw(nb, zero.policy = T)

      exp.spat$G.rcycl.obj = localG_perm(exp.spat$count_recycled, lw, nsim = 100, zero.policy = T)
      exp.spat$G.retouch.obj = localG_perm(exp.spat$count_retouched, lw, nsim = 100, zero.policy = T)

      exp.df = as.data.frame(exp.spat) %>%
        mutate(param_list[i, ])
      
      readr::write_csv(exp.df, paste0("/scratch/ec3307/recycling-Java/output/artifact-data/output/exp", i, "_", run, "_rr-local-G.csv"), num_threads=1)

      # recycled.rast = rasterFromXYZ(exp.df[,c(3,4,5)])
      # recycled.rast[recycled.rast < 2] = NA
      # retouched.rast = rasterFromXYZ(exp.df[,c(3,4,6)])
      # retouched.rast[retouched.rast < 2] = NA
      # 
      # r = overlay(recycled.rast, retouched.rast, fun=sum)
      # rcycl.ret.o = 100 - freq(r, value = NA)
      # 
      # output[nrow(output) + 1, ] <-
      #   c(param_list[i, ],
      #     run,
      #     100 - freq(recycled.rast, value = NA),
      #     100 - freq(retouched.rast, value = NA),
      #     rcycl.ret.o
      #   )
    }
  }
}

#write.csv(output, file = "recycled-retouched-overlap.csv")

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
#   geom_hline(aes(yintercept = 0), color = "red", linetype = 2, linewidth = 0.25) +
#   geom_pointrange(aes(x = var_clean, y = est, ymax = upper, ymin = lower, color = model, group = name), size = 0.1, position = position_dodge(width = 0.75)) +
#   coord_flip() +
#   scale_color_brewer(palette = "Dark2") +
#   facet_grid(name~model) +
#   labs(x = "parameter", y = "estimate") +
#   theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6), 
#         axis.title = element_text(size = 7), 
#         legend.position = "none") 
# ggsave(filename = "../figures/recycled-retouched-overlap.tiff", p, 
#        dpi = 300, width = 7, height = 3.5)
# 
# 
# hist(odata$high_recycled)
# hist(odata$high_retouched)
# 
# fit2 = zeroinfl(rcycl.ret.overlap ~ ., odata[c(15:17)])
# summary(fit2)
# 
# hist(odata$high_retouched)
# fit3 = zeroinfl(high_retouched ~ ., odata[c(2:4,6:13,16)])
# summary(fit3)
