subOTU <- function(otutab, siteInCol = TRUE, taxhead = NULL, percent = TRUE, choose = "rare",
threshold = 1, outype="Relabund", sort = TRUE, write = FALSE){
  message(paste("You select to subset the",choose,"data"))
  if (!siteInCol) {
    message("Site headers not in columns, will be transposed")
    otutab = typeConvert(otutab=as.data.frame(t(otutab)),taxhead = taxhead)}
  if(is.null(taxhead)){
    message("The input dataset does not contain a taxonomy column")
    count = otutab
  } else if(taxhead %in% names(otutab)) {
    message("There is one column given taxonomy in the data set")
    count = otutab[,which(colnames(otutab) != taxhead)]
    tax = otutab[, which(colnames(otutab) == taxhead)]
    tax = levels(tax)[tax]
  } else {stop("The taxonomy marker is incorrect, please check it")}
  if(!percent) {per = sweep(count, 2, colSums(count), "/")*100
  } else { per= count}
  if(grepl(choose,"Rare|rare")){
    ids <- which(rowMeans(per)< threshold)
  } else if (grepl(choose,"abundant")){
    ids <- which(rowMeans(per) >= threshold)
  } else if (grepl(choose,"All|all")){
    message("Keep all biosphere, but the empty rows will be removed")
    ids <- which(rowMeans(per) > 0)
  }
  otutabc <- per[ids,]
  if(!is.null(taxhead)) {taxc <- tax[ids]
  } else {taxc = NULL}
  count <- count[ids,]
  if (sort){
    ord <- order(rowMeans(otutabc), decreasing = TRUE)
    otutabc <- otutabc[ord,]
    count <- count[ord,]
    taxc <- taxc[ord]
  }
  if(grepl(outype, 'Relabund|relabund')){
    message("The outype is the relative abundance data")
    if(is.null(taxhead)) { output = per
    } else if (taxhead %in% names(otutab)){
      output <- data.frame(otutabc,taxonomy=taxc)
    } else {output = otutabc}
  } else if (grepl(outype, 'Counts|counts')){
    message("The outype is the absolute reads")
    if(is.null(taxhead)) { output = count
    } else if(taxhead %in% names(otutab)){
      output <- data.frame(count,taxc)
      names(output)[ncol(output)] = taxhead
    } else {output = count}
  } else {print("Invalid outype, will output relative abundance without taxonomy")
    output = otutabc }
  message(paste("The outype data have", dim(output)[1],"rows and",dim(output)[2],"columns"))
  print(dim(output))
  if(write){
    filename = paste0("subOtuTab_",deparse(substitute(otutab)),".txt")
    message(paste0("The subsampled table ","\"",filename,"\"", " will be written out"))
    write.table(output, file = filename, quote = FALSE, row.names = TRUE, sep = "\t")
  }
  return(output)
}

