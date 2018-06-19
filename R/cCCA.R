cCCA <- function(otutab, env, mySite=NULL,scale=FALSE,choices=1:2,display = "sites",
                 showsite = FALSE,legPos ='bottomright', saveplot = FALSE, ...){
  obj <- cca(otutab, env, scale = scale)
  axes = paste0('CCA', choices)
  xlbl = paste(axes[1], getpropX(obj, choices))
  ylbl = paste(axes[2], getpropY(obj, choices))
  pchx <- pchChoose(mySite)
  my.col <- colChoose(mySite)
  if(is.null(mySite)) {my.site = as.factor(rep('site',nrow(otutab)))
  legends = FALSE
  } else { my.site = mySite
  legends= TRUE}
  print(summary(obj))
  plot(obj, main=paste0("Constrained CCA plot for ",deparse(substitute(otutab))),
       choices = choices, cex.lab = 1.5, cex.axis=1.2,xlab = xlbl, ylab = ylbl)
  spe.sc <- scores(obj, choices = choices, scaling=2,display='sp')
  arrows(0,0, spe.sc[,1], spe.sc[,2],length=0, lty=1,col='red')
  text(obj,display="si", choices = choices, cex=0.9,col=my.col[my.site],adj=-0.1)
  if(showsite){
    sit.sc <- scores(obj, choices = choices, scaling=2, display='si')
    points(sit.sc[,1],sit.sc[,2],pch= pchx[my.site],col=my.col[my.site])
    text(sit.sc[,1],sit.sc[,2], labels=rownames(sit.sc),
         cex=0.9,col=my.col[my.site],adj=-0.1)
  }
  if(legends){
    legs <- levels(mySite)
    ncols <- ncolegend(mySite)
    yspace <- yinterspace(mySite)
    legend(legPos, legend = legs, bty="o", cex=1.5, pch= pchx,
           col = my.col, ncol = ncols, y.intersp = yspace)
  }
  if(saveplot)
    dev.copy2pdf(file = paste0("Constrained_CCA_plot_",deparse(substitute(otutab)),".pdf"),
                 useDingbats=FALSE)
}
