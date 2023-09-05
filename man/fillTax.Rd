\name{fillTax}
\alias{fillTax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  reshape the heirarchical taxonomy
}
\description{
reformat the taxonomy with the last clear assignment and changing the prefix
}
\usage{
fillTax(x, split = ';', prefix=TRUE, fillAll=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character, structured strings giving the heirarchical rank of taxonomy, e.g., d__Bacteria;
p__Planctomycetota;c__Planctomycetes;o__Gemmatales;f__Gemmataceae;g__;s__}
  \item{split}{
Character, the seperator for the heirarchical taxonomy.
}
  \item{prefix}{
Logical, whether contains prefix in the taxonomic strings, default TRUE.
}
  \item{fillAll}{
Logical, whether to fill all taxonic level, default TRUE.
}}
\seealso{
fillTax2, slimTax
}
\examples{
  test = 'd__Bacteria;p__Planctomycetota;c__Planctomycetes;o__Gemmatales;f__Gemmataceae;g__;s__'
  fillTax(x = test, split = ';', prefix=TRUE, fillAll=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summarize }% use one of  RShowDoc("KEYWORDS")

