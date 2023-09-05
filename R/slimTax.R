slimTax <- function(x, from, to, separator =';', jump=FALSE){
  ## this only works for the taxonomy with all levels
  ## pre = c("d","p","c","o","f","g",'s'), meaning 7 levels in total
  ## jump means only take the tax of from and to, excluding those in between
  fillTaxAll <- function(taxStr, split = ';', prefix=TRUE){
    tax = strsplit(taxStr, split)
    ## tax is a list of strings
    taxlevel = c("d","p","c","o","f","g",'s')
    lasTax <- function(st, pref = prefix){
	if (pref) {
	    y = gsub('(.*?)__(.*)','\\2', st)
          if(length(y)!= 7) y = c(y, '')
	} else {
	    y = st;
	    pre = taxlevel
	}
	if(!is.na(match('',y))){
	    ## find the index of first match with none 'taxonomy'
          id = match('',y)
	    y[id:length(taxlevel)] = y[id-1]
	    pre = taxlevel ## keep pre as the last top point
	    out = paste(pre, y, sep='__')
	} else { ## there is no missing tax
	    out = st
	}
	return(out)
	}
    res = lapply(tax, function(x) lasTax(st = x, pref= prefix))
    res = sapply(res, function(x) paste(x, collapse=';'))  ## join into one long-string
    return(res)
  }
  tax = fillTaxAll(x)
  if(jump) out = sapply(strsplit(tax, split= separator), 
			function(x) paste(x[c(from,to)], collapse=separator))
  else out = sapply(strsplit(tax, split= separator), 
		    function(x) paste(x[from:to], collapse=separator))
  return(out)
}
