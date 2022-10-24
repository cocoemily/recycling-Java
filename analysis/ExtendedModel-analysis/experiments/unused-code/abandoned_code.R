library(corrplot)

plot_correlogram = function(cordata, title) { 
  cormat = cor(cordata, use = "complete.obs")
  
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(cordata)
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  return(corrplot(cormat, method="color", col=col(200),  
                  type="upper", order="hclust", 
                  addCoef.col = "black", # Add coefficient of correlation
                  tl.col="black", tl.srt=45, #Text label color and rotation
                  # Combine with significance
                  p.mat = p.mat, sig.level = 0.05, insig = "blank",
                  number.cex = 0.5, 
                  # hide correlation coefficient on the principal diagonal
                  diag=FALSE, title=title
  )
  )
}



for(p in parameters) {
  values = c(unique(layerdata[p]))
  for(v in values[[1]]) {
    cordata = layerdata %>% filter(!!as.name(p) == v) %>% select(outputs)
    title = paste(p, ":", v)
    plot_correlogram(cordata, title)
  }
}


#possibly add in some indication of how different correlation matrices are based on parameters
#r values to z scores between recycling and each of the outputs
#regression on z scores?

cor.by.exp = layerdata %>% group_by_at(vars(one_of(parameters))) %>% 
  summarize(ri.cr.cor = cor(recycling.intensity, cortex.ratio, use ="complete.obs"),
            ri.nc.cor = cor(recycling.intensity, nodule.count, use ="complete.obs"),
            ri.fc.cor = cor(recycling.intensity, flake.count, use ="complete.obs"),
            ri.nd.cor = cor(recycling.intensity, num.discards, use ="complete.obs"),
            ri.ns.cor = cor(recycling.intensity, num.scavenge, use ="complete.obs"),
            ri.ne.cor = cor(recycling.intensity, num.encounters, use ="complete.obs"),
            ri.nm.cor = cor(recycling.intensity, num.manufacture, use ="complete.obs"),
            ri.nr.cor = cor(recycling.intensity, num.retouch, use ="complete.obs"),
            ri.no.cor = cor(recycling.intensity, num.retouch, use ="complete.obs"),
            ri.av.cor = cor(recycling.intensity, assemblage.vol, use ="complete.obs"),
  )