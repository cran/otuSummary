rareBiosphere <- function(otutab, siteInCol = TRUE, taxhead= NULL, percent=FALSE, threshold = 1, cutRatio = 100, cutPERare = 5, ...) {
  if(percent) stop("This function only work for OTUs with absolute reads!")

  if(!siteInCol){
    otutab <- typeConvert(otutab=as.data.frame(t(otutab)),taxhead = taxhead)}
  if(!is.null(taxhead)) {
    data <- otutab[,which(colnames(otutab) != taxhead)]
    tax  <- otutab[,which(colnames(otutab) == taxhead)]
  } else data <- otutab

  per <- sweep(data,2,colSums(data),"/")*100
  cutoff <- (which(rowMeans(per)< threshold))
  rare <- per[cutoff, ]
  rare_count <- data[cutoff, ]

  rowsum = rowSums(rare_count)
  single = vector(mode="character", length=length(rowsum))
  for (z in seq_along(rowsum)) {
    if (rowsum[z] ==1) single[z] = "Absolute_singleton"
    else if (rowsum[z] == 2) single[z] = "Doubleton"
    else single[z] = "Not"
  }

  len <- apply(rare, 1, function(x) sum(x>0))
  mx  <- apply(rare, 1, function(x) max(x))
  mn  <- apply(rare, 1, function(x) min(x[x>0]))
  for (k in seq_along(len)) {
    if (len[k] ==1) mn[k] = 0
    else mn
  }
  ratio = rep(-1,times=length(len))
  for (i in seq_along(len)) {
    if (len[i] == 1)  next
    else   ratio[i] = mx[i]/mn[i]
  }

  identifier = vector(mode="character", length=length(ratio))
  for (i in 1:length(ratio)) {
    if (ratio[i] > cutRatio)  identifier[i] = 'CRT'
    else if (ratio[i] < cutPERare)  identifier[i] = 'PERare'
    else identifier[i] = 'OtherRare'
  }
  if(!is.null(taxhead)) {tax2 <- tax[cutoff]
    smy <- data.frame(max_rabund = mx, min_rabund = mn,ratio, RareType = identifier,singleton=single,taxonomy=tax2)
  } else {
    smy <- data.frame(max_rabund = mx, min_rabund = mn,ratio, RareType = identifier,singleton=single)}
  smy[smy$ratio == -1,'ratio'] <- 'n.a.'
  id.crt <- identifier[identifier == 'CRT']
  sumTab = table(identifier)
  print(paste("Number CRT are", as.vector(sumTab["CRT"])))
  print(paste("Number PERare are", as.vector(sumTab["PERare"])))
  print(paste("Number OtheRare are", as.vector(sumTab["OtherRare"])))
  print(paste("Number absolute singletons are", sum(smy$ratio == 'n.a.')))
  print(paste("Number doubletons are", sum(len==2)))

  out = data.frame(rare_count, smy)
  output = vector("list", length = 4)
  output[[1]] <- smy
  types = c("CRT","PERare","OtherRare")
  nms <- c("summaryTable","CRT","PERare","otherRare")
  for(i in 1:length(types)) {
    output[[i+1]] <- out[identifier == types[i],]
  }
  names(output) <- nms
  return(output)
}
