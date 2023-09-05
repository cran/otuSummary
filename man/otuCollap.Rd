\name{otuCollap}
\alias{otuCollap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
collapse a OTU table at given level.
}
\description{
The function will collapse a structured OTU table at given taxonomic level.
}
\usage{
otuCollap(otutab, taxto, siteInCol = TRUE, taxhead = "taxonomy",
    pattern = ";", collap = ";")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otutab}{
A OTU table of microbial community, which must contain a taxonomic column (if siteInCol) or row (if site in rows). The otu table can be given in numeric counts or in relative abundance.
}
  \item{taxto}{
numeric, collapse the otutable at the taxonomic level which start position (index) from the left  of taxonomy. For example, in "Archaea; Euryarchaeota; Methanomicrobia; Methanomicrobiales; Methanoregulaceae", the indices of "Euryarchaeota" and "Methanomicrobiales" are 2 and 4, respectively.
}
  \item{siteInCol}{
Logical, if "TRUE", the OTU table contains samples in columns and taxa in rows. The function will decide whether to transpose the otu table based on this parameter.
}
  \item{taxhead}{
Character, specify the header of taxonomy. By default we assume your taxonomic column is entitled "taxonomy".
}
  \item{pattern}{
Character, specify the separation of taxonomy. By default, the taxonomy is separated by semicolon (";").
}
  \item{collap}{
Character, tell the function about the separation for the hierarchical order in the output.
}
}
\details{
This function will directly collapse the otu table according to numeric position of structured taxonony. This function can also collapse data with structured format like the example OTU table.
}
\value{
The function will return a collapsed OTU table.
%%  If it is a LIST, use
%% ...
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
otuReport
}
\examples{

data(otuqiime)
dim(otuqiime)

result <- otuCollap(otutab = otuqiime, taxto = 2, siteInCol = TRUE,
    taxhead = "taxonomy", pattern = ";")

dim(result)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summarize }% use one of  RShowDoc('KEYWORDS')
