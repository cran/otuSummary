matrixConvert <- function(triMatrix, colname = c("sp1","sp2","dist")){
  m <- as.matrix(triMatrix)
  m2 <- reshape2::melt(m)[reshape2::melt(upper.tri(m))$value,]
  names(m2) <- colname
  invisible(m2)
}

