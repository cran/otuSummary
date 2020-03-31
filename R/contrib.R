contrib <- function(otutab, siteInCol=TRUE, taxhead=NULL, threshold = 1, percent=FALSE, check="rare", write=FALSE, plot=FALSE, ...) {
  if (!siteInCol) {
    message("Site headers in columns, will be transposed")
    otutab = typeConvert(otutab=data.frame(t(otutab),stringsAsFactors = FALSE),taxhead = taxhead)
  }
  if (!is.null(taxhead)){
    message("The taxonomy column will be not used in calculating the Bray contribution")
    otutab = otutab[,-match(taxhead, colnames(otutab))]
  }
  otutab = data.frame(t(otutab),stringsAsFactors = FALSE)
  if(!percent) {relabund <- sweep(otutab,1,rowSums(otutab),"/")*100
  } else {relabund = otutab}
  if (grepl(check,"R|rare")) {
    message("You choose the rare biosphere")
    sub <- relabund[,colMeans(relabund)< threshold]
    sub <- sub[,colSums(sub) > 0]
  } else if (grepl(check,"A|abundant")) {
    message("You choose the abundant biosphere")
    sub <- relabund[,colMeans(relabund) >= threshold]
    sub <- sub[,colSums(sub) > 0]
  } else {stop("Invalid choice! Work only for abundant and rare biosphere")}
  print(paste("Dimemsion of subsampled taxa table", dim(sub)[1],"rows,",dim(sub)[2], "columns"))
  distp <- pbray(allComm = relabund, subComm = sub)
  distot <- vegdist(relabund, method ='bray')
  r1 <- matrixConvert(distp,colname = c("sample1","sample2","pBrayCurtis"))
  r2 <- matrixConvert(distot,colname = c("sample1","sample2","totalBrayCurtis"))
  r3 <- matrixConvert(distp/distot,colname = c("sample1","sample2","Contribution"))
  result <- data.frame(r2, pBrayCurtis=r1[,3],Contribution = r3[,3])
  if(write){
    filenm <- paste0("Contrib_",check,"_2BC_at_",threshold, "%",".txt")
    write.table(result,file = filenm, quote = FALSE, sep = "\t")
  }
  if (plot) {
    boxplot(r3[,3],col="steelblue", ...)
    text(1+0.26, summary(r3[,3]),labels = round(summary(r3[,3]),4),col='blue')
  }
  invisible(result)
}
