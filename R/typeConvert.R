typeConvert <- function(otutab,taxhead = NULL){
  if (!is.null(taxhead)) {
    otu <- otutab[,which(colnames(otutab) != taxhead)]
    tax <- otutab[,which(colnames(otutab) == taxhead)]
    indx <- sapply(otu, is.factor)
    otu[indx] <- lapply(otu[indx], function(x) as.character(x))
    otutab <- data.frame(otu,tax)
    names(otutab)[ncol(otutab)] <- taxhead
  } else {
    indx <- sapply(otutab, is.factor)
    otutab[indx] <- lapply(otutab[indx], function(x) as.character(x))
  }
  stopifnot(is.data.frame(otutab))
  otutab[] <- rapply(otutab, type.convert, classes = "character", how = "replace", as.is = TRUE)
  return(otutab)
}
