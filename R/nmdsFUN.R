nmdsFUN <- function(otutab, env=NULL, mySite=NULL, choices=1:2, display = "sites",
                    showsp=TRUE, legPos ='bottomright', saveplot=FALSE, ...){
  set.seed(2)
  obj <- metaMDS(otutab, k = 3,trymax = 25, distance="bray")
  pchx = pchChoose(mySite)
  my.col = colChoose(mySite)
  if(is.null(mySite)) {my.site = as.factor(rep("site",nrow(otutab)))
  legends = FALSE
  } else { my.site = mySite
  legends = TRUE}
  print(summary(obj))
  axes = paste0("NMDS", choices)
  xlbl = paste(axes[1], getpropX(obj, choices))
  ylbl = paste(axes[2], getpropY(obj, choices))
  if(is.null(env)){
    plot(obj,type="n",xlab = xlbl, ylab = ylbl, cex.lab = 1.5,
         cex.axis=1.2, display = display, choices = choices,
         main = paste0("NMDs plot for ",deparse(substitute(otutab))))
    abline(h=0,v=0,lty="dashed",col="grey")
    points(obj, display="si", choices = choices, cex=1.2,pch= pchx[my.site],
           col=my.col[my.site],bg=my.col[my.site])
    text(obj,display="si", choices = choices, cex=0.9,col=my.col[my.site],adj=-0.1)
    if(showsp){
      points(obj, display="sp", choices = choices, cex=0.9,pch= '+',col='red')
      text(obj, display="sp", choices = choices, cex=0.9,col = my.col[my.site],adj=-0.1)
    }
    if(legends){
      legs = levels(mySite)
      ncols = ncolegend(mySite)
      yspace = yinterspace(mySite)
      legend(legPos, legend = legs, bty="o", cex=1.5, pch= pchx, col=my.col, ncol = ncols, y.intersp = yspace)
      text(locator(),paste0("Stress=", round(obj$stress,4)),cex=0.8)
    }
    if(saveplot)
      dev.copy2pdf(file = paste0("NMDs_plots_",deparse(substitute(otutab)),".pdf"), useDingbats=FALSE)
  } else{
    efit <- envfit(obj, env, permutations = 999, strata = NULL,choices=c(1,2), display = "sites", w = weights(obj), na.rm = FALSE)
    plot(obj,type="n",xlab = xlbl, ylab = ylbl, cex.lab = 1.5, cex.axis=1.2, choices = choices,
         main = paste0("NMDs plot with env for ",deparse(substitute(otutab))))
    points(obj, display="si",choices = choices, cex=1.2,pch= pchx[my.site],col=my.col[my.site],bg=my.col[my.site])
    text(obj, display="si", choices = choices, cex=0.9,col=my.col[my.site],adj=-0.1)
    plot(efit, col='red', p.max = 0.05)
    if(showsp){
      points(obj, display = "sp", choices = choices, cex = 0.9,pch = '+',col = 'red')
      text(obj, display = "sp", choices = choices, cex = 0.9, col = my.col[my.site],adj = -0.1)
    }
    if(legends){
      legs = levels(mySite)
      ncols = ncolegend(mySite)
      yspace = yinterspace(mySite)
      legend(legPos, legend = legs, bty = "o", cex=1.5, pch= pchx,
             col=my.col, ncol = ncols, y.intersp = yspace)
      text(locator(),paste0("Stress=", round(obj$stress,4)),cex=0.8)
    }
    if(saveplot)
      dev.copy2pdf(file = paste0("NMDs_plots_",deparse(substitute(otutab)),".pdf"),
                   useDingbats=FALSE)
  }
}
