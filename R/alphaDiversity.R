alphaDiversity <- function(otutab, siteInCol = FALSE, taxhead = NULL, threshold = 1, percent = FALSE, write = FALSE, ...){
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
  if(!percent){
    per <- sweep(otutab, 1, rowSums(otutab),"/")*100
  } else per <- otutab

  abund = per[ , colMeans(per) >= threshold]
  rare  = per[ , colMeans(per) < threshold ]

  data <- list(per, abund, rare)
  result <- vector("list",length = 3)
  result <- lapply(1:3, function(i) data.frame(
    observed = apply(data[[i]], 1, function(x) sum(x>0)),
    shannon = apply(data[[i]], 1, function(x) calc_shannon(data=x,base=exp(1))),
    simperson = apply(data[[i]], 1, function(x) calc_simpson(data=x,index ='simpson')),
    invsimperson = apply(data[[i]], 1, function(x) calc_simpson(data=x,index ='invsimpson')),
    chao1 = apply(data[[i]], 1, function(x) calc_chao1(x)),
    chao2 = apply(data[[i]], 1, function(x) calc_chao2(x)),
    evenness = apply(data[[i]], 1, function(x) sum(x>0))/apply(data[[i]], 1, function(x) calc_shannon(data=x,base=exp(1)))
  ))
  names(result) = c("allBio","abundBio","rareBio")
  if(write){
    sapply(1:3, function(i) write.table(
      result[[i]],file = paste0("Alpha_diversity_",names(result)[i],".txt"),
      quote = FALSE, sep = "\t", ...))
  }
  return(result)
}
