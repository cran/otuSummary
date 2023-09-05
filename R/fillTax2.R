fillTax2 <- function(x, split = ';', prefix=TRUE){
    tax = strsplit(x, split)
    ## tax is a list of strings

    lasTax <- function(st, pref = prefix){
	if (pref) {
	    y = gsub('(.*?)__(.*)','\\2', st)
          pre = gsub('(.*?)__(.*)','\\1', st)
	} else {
	    y = st;
	    pre = c("d","p","c","o","f","g",'s')
	    pre = pre[1:length(y)]
	}
	if(!is.na(match('',y))){
	    ## find the index of first match with none 'taxonomy'
          id = match('',y)
	    y[id:length(y)] = y[id-1]
	    pre[id:length(y)] = pre[id-1] ## keep pre as the last top point
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
