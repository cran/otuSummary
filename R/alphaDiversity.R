alphaDiversity <- function(otutab, siteInCol = FALSE, taxhead = NULL, threshold = 1,
                           percent = FALSE, write = FALSE, ...){
  message("This function accepts data with Site in rows")
  if(siteInCol) {
    if(!is.null(taxhead)) {
      otutab = otutab[, which(colnames(otutab) != taxhead)]
      otutab = typeConvert(otutab=as.data.frame(t(otutab)),taxhead = NULL)
      } else otutab = typeConvert(otutab=as.data.frame(t(otutab)),taxhead = NULL)
  } else {
    if(!is.null(taxhead)) {
      otutab = otutab[which(rownames(otutab) != taxhead), ]
      otutab = typeConvert(otutab = otutab,taxhead = NULL)
     } else otutab = otutab
  }
  if(percent){warning("Percent data are risky to calculate alpha diversity indices!")
    per <- otutab
    } else {
    per <- sweep(otutab, 1, rowSums(otutab),"/")*100
  }

  comAll = otutab
  abund = comAll[ , colMeans(per) >= threshold]
  rare  = comAll[ , colMeans(per) < threshold ]
  print(dim(abund))
  print(dim(rare))

  data <- list(comAll, abund, rare)
  result <- vector("list",length = 3)
  result <- lapply(1:3, function(i) data.frame(
    observed = specnumber(data[[i]]),
    shannon = diversity(data[[i]], "shannon"),
    simperson = diversity(data[[i]], "simpson"),
    invsimperson = diversity(data[[i]], "invsimpson"),
    chao1 = apply(data[[i]], 1, function(x) vegan::estimateR(x))[2,],
    chao2 = apply(data[[i]], 1, function(x) chao2(x)),
    evenness = diversity(data[[i]], "shannon")/specnumber(data[[i]]),
    Gini = apply(data[[i]], 1, function(x) gini(x))
  ))
  names(result) = c("allBio","abundBio","rareBio")
  if(write){
    sapply(1:3, function(i) write.table(
      result[[i]],file = paste0("Alpha_diversity_",names(result)[i],".txt"),
      quote = FALSE, sep = "\t", ...))
  }
  return(result)
}
