seperation <- function(data, pattern){
  if (any(grepl(pattern,data))) {out = paste(data)
  } else out = paste0(data,pattern)
  return(as.factor(out))
}
