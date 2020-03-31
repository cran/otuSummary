otuReport <- function(otutab, siteInCol=TRUE, taxhead='taxonomy', platform ='mothur', pattern=';', prefix=TRUE, percent=FALSE, taxlevel='phylum', collap = "->"){
  message(rep('= = ',times = 15))
  message('Data checking ... ')
  taxlab = tolower(taxhead)
  colnms = tolower(colnames(otutab))
  if (!siteInCol) {
    message("Site headers not in columns, will be transposed")
    otutab = typeConvert(otutab = data.frame(t(otutab), stringsAsFactors = FALSE), taxhead = taxhead)}
  if(!any(grepl(taxlab,colnms))) {
    stop(paste0("The input data have not a column named '",taxhead,"'"))}
  message(rep('- - ',times = 15))
  message('This funciton is designed for OTU table')
  message("OTUs in rows with a column given taxonomy")
  message(paste('The input OTUs is in',toupper(platform), 'format'))
  message(rep('- - ',times = 15))
  taxa = otutab[, which(colnms == taxlab)]
  counts = otutab[, -which(colnms == taxlab)]
  if(platform=='qiime'){prefx = NULL
  } else{ if(prefix){prefx = c('k__','p__','c__','o__','f__','g__','s__')
  }else{prefx = NULL}}
  lineage = vector('list', length = 7)
  if(platform == 'mothur'){
    taxa = gsub('\\(\\d+\\)','',taxa)
    lineage = lapply(1:7, function(i)
      sapply(strsplit(as.character(taxa), split = pattern),
             function(x) paste(prefx[1:i],x[1:i],sep='',collapse = collap)))
  } else if (platform == 'qiime'){
    taxa = gsub('\\(\\d+\\)','',taxa)
    taxa = gsub('\\[|\\]','',taxa)
    taxa = gsub('; ',';', taxa)
    taxa = sapply(taxa, function(x) seperation(x,pattern))
    lineage = lapply(1:7, function(i)
      sapply(strsplit(as.character(taxa), split = pattern),
             function(x) paste(x[1:i],sep='',collapse=collap)))
  }
  names(lineage)= c('kingdom','phylum','class','order','family','genus','species')
  TaxaFreq = sapply(lineage, table)
  taxaFrac = sapply(TaxaFreq,function(x) x/sum(x)*100)
  comm = vector('list', length = 7)
  comm = lapply(1:7, function(i)
    apply(counts, 2, function (x)
      tapply(x, INDEX = lineage[[i]], sum)))
  names(comm)= c('kingdom','phylum','class','order','family','genus','species')
  if (!percent){
    commOne = lapply(comm, function(x) SUM(x))
    readFrac = sapply(comm,function(x) x/sum(x)*100)
    Relabund = lapply(comm, function(x) SWEEP(x))
    readFracOne = sapply(readFrac, SUM)
    RelabundMean = sapply(Relabund, MEAN)
  } else {
    commOne = lapply(comm, function(x) SUM(x))
    readFrac = NULL
    readFracOne = sapply(comm, MEAN)
  }
  message(rep('= = ',times = 15))
  message(paste('The Summary Information at Rank of',toupper(taxlevel)))
  if(!percent){
    taxlevel = tolower(taxlevel)
    whatfound = unique(lineage[[taxlevel]])
    whatFreq = TaxaFreq[[taxlevel]]
    whatFrac = taxaFrac[[taxlevel]]
    readsSitewise = comm[[taxlevel]]
    readsOne = commOne[[taxlevel]]
    readsFracSitewise = readFrac[[taxlevel]]
    readsFracOne = readFracOne[[taxlevel]]
    RelabundSitewise = Relabund[[taxlevel]]
    Relabund_mean = sort(RelabundMean[[taxlevel]], decreasing=TRUE)
    result = list(whatfound,whatFreq,whatFrac,readsSitewise,readsOne,
                  readsFracSitewise,readsFracOne,RelabundSitewise,Relabund_mean)
    names(result) = c("whatTaxa","taxaFreqs","taxaFrac",
                      "reads","readSum",
                      "readFrac","readFracSum",
                      "Relabund","RelabundMean")
    rm(lineage, comm)
  } else {
    taxlevel = tolower(taxlevel)
    whatfound = unique(lineage[[taxlevel]])
    whatFreq = TaxaFreq[[taxlevel]]
    whatFrac = taxaFrac[[taxlevel]]
    readsSitewise = comm[[taxlevel]]
    readsFracOne = sort(readFracOne[[taxlevel]],decreasing=TRUE)
    result = list(whatfound,whatFreq,whatFrac,readsSitewise,readsFracOne)
    names(result) = c("whatTaxa","taxaFreqs","taxaFrac",
                      "Relabund","RelabundMean")
    rm(lineage, comm)
  }
  message(paste("Printing summary at Rank of",toupper(taxlevel),"below"))
  message(rep('= = ',times = 15))
  return(result)
}




