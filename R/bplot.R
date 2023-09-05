bplot <- function(data, srt = 45, yoff = 0.05, dataoff = 0.025, barcol="grey", grid = TRUE, ...){
  message("This function only works with numeric vector or table object")
  if (is.vector(data)|is.table(data)) {
    a <- barplot(height = data, space=0.75, ylim = range(0, 1.1*max(data)),
                 width = 0.80,axes = FALSE, xaxt="n", col = barcol, ...)
    if(grid)  grid(nx = NA, ny = NULL)
    axis(2, labels = TRUE, las=1, ...)
    if(!is.null(names(data))){
      axis(1, labels = FALSE, at = a, ...)
      text(x = a, y = par("usr")[3] - yoff*max(data), labels= names(data), srt = srt,
           xpd = TRUE, adj = c(1,0.5), cex = 0.8, font= 3, ...)
    }
    text(x = a, y = data + dataoff*max(data), labels = data, ...)
    box()
    rm(a)
  } else stop('Input data should be a vector or a table object in this function')
}
