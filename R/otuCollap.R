otuCollap <- function(otutab, taxto, siteInCol = TRUE, taxhead='taxonomy', pattern=';', collap=";"){
  if (!siteInCol) {
    message("Site headers not in columns, will be transposed")
    otutab = typeConvert(otutab = data.frame(t(otutab), stringsAsFactors = FALSE), taxhead = taxhead)
    }
  taxlab = taxhead
  colnms = colnames(otutab)
  if(!any(grepl(taxlab,colnms))) {
    stop(paste0("The input data have not a column named '",taxhead,"'"))
    }
  taxa = as.vector(otutab[, grepl(taxlab, colnms, ignore.case=TRUE)])
  counts = otutab[, !grepl(taxlab, colnms, ignore.case=TRUE)]
  taxa = gsub('\\(\\d+\\)','',taxa)
  linege = sapply(strsplit(as.character(taxa), split = pattern),
                  function(x) paste(x[1:taxto],sep='',collapse = collap))
  abundance = apply(counts, 2, function(x) tapply(x, INDEX = linege, sum))
  Taxonomy = attr(abundance, 'dimnames')[[1]]
  result = data.frame(taxonomy = Taxonomy, abundance, stringsAsFactors = FALSE)
  rownames(result) = NULL
  return(result)
}
