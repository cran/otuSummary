ordination <- function(otutab, env=NULL, SiteInRow=TRUE, percent=TRUE, mySite=NULL, ordtype ='urda',
                       scale = TRUE, display = "sites", choices = 1:2, biplot=FALSE,
                       legPos ='bottomright', showsite = FALSE, saveplot = FALSE, ...){
  message(rep('=',70))
  message("For unconstained ordination, ordtype options c('urda','ucca','unmds')")
  message("For constained ordination, ordtype options c('c_rda','c_cca','c_nmds')")
  message('For quick check only, please use original vegan functions for better plots')
  message(rep('=', 70))
  if(!SiteInRow) otutab <- typeConvert(otutab=data.frame(t(otutab),stringsAsFactors = FALSE),taxhead = NULL)
  if(!percent) otutab <- sweep(otutab, 1, rowSums(otutab),"/")*100
  my.col = colChoose(mySite)
  pchx = pchChoose(mySite)
  if(is.null(env)){
    message(rep('-',60))
    message('Unconstrained ordination will be processed')
    if(grepl(ordtype, "urda")){
      uRDA(otutab,scale= scale, choices = choices,
           display = display, showsp=TRUE, ...)
    } else if(grepl(ordtype, "ucca")){
      uCCA(otutab,scale = scale, choices = choices,
           display = display, showsp=TRUE, ...)
    } else if(grepl(ordtype, "unmds")){
      nmdsFUN(otutab,scale = scale, choices = choices,
              display = display, showsp=TRUE, ...)
    }else {stop("invalid choice! Please read function tutorial")
    }
  } else{
    message(rep('-',60))
    message('Constrained ordination will be processed')
    if(grepl(ordtype, "c_cca")){
      cCCA(otutab,env,scale = scale,choices = choices,
           display = display, showsp=TRUE, ...)
    } else if(grepl(ordtype, "c_rda")){
      cRDA(otutab,env, scale = scale,choices = choices,
           display = display, showsp=TRUE, ...)
    } else if(grepl(ordtype, "c_nmds")){
      nmdsFUN(otutab, env, scale = scale,choices = choices,
              display = display, showsp=TRUE, ...)
    }else {stop("invalid choice! Please read function tutorial")
    }
  }
  message("Please check the output file")
}
