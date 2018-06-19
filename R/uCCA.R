uCCA <- function(otutab, scale = FALSE, mySite = NULL, plot = TRUE, choices = 1:2,
                 biplot = FALSE, display = "sites", showsp = TRUE, legPos ='bottomright',
                 saveplot = FALSE, ...){
  obj <- cca(otutab, scale = scale)
  print(summary(obj))

  pchx = pchChoose(mySite)
  my.col = colChoose(mySite)
  axes = paste0('CA', choices)
  xlbl = paste(axes[1], getpropX(obj, choices))
  ylbl = paste(axes[2], getpropY(obj, choices))

  if(is.null(mySite)) {my.site = as.factor(rep('site',nrow(otutab)))
  legends = FALSE
  } else { my.site = mySite
  legends= TRUE}

  if(biplot){
    biplot(obj, scaling = 3, choices = choices, cex.lab=1.5,cex.axis=1.2,
           main = "Unconstrained CA biplot - scaling 3", choices=choices)
    if(legends){
      legs = levels(mySite)
      ncols = ncolegend(mySite)
      yspace = yinterspace(mySite)
      legend(legPos, legend = legs, bty="o", cex=1.5, pch= pchx,
             col=my.col, ncol = ncols, y.intersp = yspace)
    }
    if(saveplot)
      dev.copy2pdf(file = paste0("Unconstained_CA_plot_",deparse(substitute(otutab)),".pdf"),
                   useDingbats = FALSE)
  }else{
    plot(obj,type="n",xlab = xlbl, ylab = ylbl, cex.lab = 1.5, cex.axis=1.2,choices = choices,
         main = paste0("Unconstrained CA plot for ",deparse(substitute(otutab))), ...)
    abline(h=0,v=0,lty="dashed", col="grey")
    points(obj, display="sites", choices = choices, cex=1.2, pch= pchx[my.site],
           col=my.col[my.site], bg=my.col[my.site], choices=choices)
    text(obj,display="sites", choices = choices, cex=0.9, col=my.col[my.site],adj=-0.1)
    if(showsp){
      points(obj, display="sp",choices = choices, cex=0.9,pch= '+',col='red', ...)
      text(obj,display="sp", choices = choices, cex=0.9,col = my.col[my.site],adj=-0.1)
    }
    if(legends){
      legs = levels(mySite)
      ncols = ncolegend(mySite)
      yspace = yinterspace(mySite)
      legend(legPos, legend = legs, bty="o", cex=1.5, pch= pchx,
             col=my.col, ncol = ncols, y.intersp = yspace)
    }
    if(saveplot)
      dev.copy2pdf(file = paste0("Unconstained_CA_plot_",deparse(substitute(otutab)),".pdf"),
                   useDingbats = FALSE)
  }
}
