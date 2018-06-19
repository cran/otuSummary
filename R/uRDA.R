uRDA <- function(otutab,env = NULL, mySite=NULL, scale=FALSE, biplot=FALSE, choices=1:2,
                 display = "sites", showsp=TRUE, legPos ='bottomright', saveplot=FALSE, ...){
  obj <- rda(otutab, scale = scale)
  print(summary(obj))
  axes = paste0('PC', choices)
  xlbl = paste(axes[1], getpropX(obj, choices))
  ylbl = paste(axes[2], getpropY(obj, choices))
  pchx = pchChoose(mySite)
  my.col = colChoose(mySite)
  if(is.null(mySite)) {my.site = as.factor(rep('site', nrow(otutab)))
  legends = FALSE
  } else { my.site = mySite
  legends= TRUE}
  if(biplot){
    biplot(obj, scaling = 3, choices = choices, type = c("text", "points"), cex.lab=1.5,cex.axis=1.2,
           main = "Unconstrained PCA biplot - scaling 3")
    if(legends){
      legs = levels(mySite)
      ncols = ncolegend(mySite)
      yspace = yinterspace(mySite)
      legend(legPos, legend = legs, bty="o", cex=1.5, pch= pchx,
             col=my.col, ncol = ncols, y.intersp = yspace)
    }
    if(saveplot){
      dev.copy2pdf(file = paste0("Unconstained_RDA_plot_",deparse(substitute(otutab)),".pdf"),
                   useDingbats = FALSE)
    }
  }else{
    plot(obj,type="n",xlab = xlbl, ylab = ylbl, cex.lab = 1.5, cex.axis=1.2, choices=choices,
         main = paste0("Unconstrained RDA plot for ", deparse(substitute(otutab))))
    abline(h=0,v=0,lty="dashed",col="grey")
    points(obj, display="si",cex=1.2,pch= pchx[my.site], choices = choices,
           col=my.col[my.site], bg=my.col[my.site], ...)
    text(obj,display="si", choices = choices, cex=0.9,col=my.col[my.site],adj=-0.1, ...)
    if(showsp){
      points(obj, display="sp",choices = choices, cex=0.8, pch= '+', col='red')
      text(obj, display="sp", choices = choices, cex=0.9, col = my.col[my.site],adj=-0.1)
    }
    if(legends){
      legs = levels(mySite)
      ncols = ncolegend(mySite)
      yspace = yinterspace(mySite)
      legend(legPos, legend = legs, bty="o", cex=1.5, pch= pchx,
             col=my.col, ncol = ncols, y.intersp = yspace)
    }
    if(saveplot)
      dev.copy2pdf(file = paste0("Unconstained_RDA_plot_",deparse(substitute(otutab)),".pdf"),useDingbats =F)
  }
}
