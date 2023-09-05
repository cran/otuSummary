\name{fillTax2}
\alias{fillTax2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  reshape the heirarchical taxonomy
}
\description{
this function is similar to fillTax, except that this function has no parameter of "fillAll" 
}
\usage{
fillTax2(x, split = ';', prefix=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character, structured strings giving the heirarchical rank of taxonomy. Please convert to character object for your input taxonomic information before using.
  }
  \item{split}{
Character, the seperator for the heirarchical taxonomy.
  }
  \item{prefix}{
Logical, whether contains prefix in the taxonomic strings, default TRUE.
  }
}
\seealso{
fillTax, slimTax
}
\examples{
  test = 'd__Bacteria;p__Planctomycetota;c__Planctomycetes;o__Gemmatales;f__Gemmataceae;g__;s__'
  fillTax2(x = test, split = ';', prefix=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summarize }% use one of  RShowDoc("KEYWORDS")

