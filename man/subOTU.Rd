\name{subOTU}
\alias{subOTU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subset a OTU table
}
\description{
This function subset a OTU table according to the specified threshold of mean relative abundance.
}
\usage{
subOTU(otutab, siteInCol = TRUE, taxhead = NULL, percent = TRUE, choose = "rare",
       threshold = 1, outype = "Relabund", sort = TRUE, write = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otutab}{
An OTU table of microbial community, which can contain a taxonomic column (if siteInCol) or row (if site in rows). The OTU table can be given in numeric counts or in relative abundance.
}
  \item{siteInCol}{
Logical, if "TRUE", the OTU table contains samples in columns and taxa in rows. The function will decide whether to transpose the OTU table based on this parameter.
}
  \item{taxhead}{
Character, specify the header of taxonomy. By default the taxonomic column is NULL.
}
  \item{percent}{
Logical, whether the input OTU table is in relative abundance. The default is TRUE.
}
  \item{choose}{
Character, tell the function which part to subset out. The valid choice are "rare","abundant" and "all", which will specify the rare biosphere, the abundant population and keep the whole community data.
}
  \item{threshold}{
Numeric, the threshold specify the relative abundance cutoff upon which the rare biosphere is subset.
}
  \item{outype}{
Character, specify whether the output otu subset should in relative abundance (outype="Relabund" or "relabund") or in absolute counts (outype="counts" or "Counts"). This function also support partial match of the parameter.
}
  \item{sort}{
Logical, be default, the output OTU subset is sorted according to the descending order of mean relative abundance across samples.
}
  \item{write}{
Logical, whether the output OTU table will be written out when running this function. The default is FALSE.
}
}
\details{
The function will subset the OTU table to abundant (choose = "abundant") or rare biosphere (choose = "rare") according to the given relative abundance threshold of rare biosphere (Lynch and Neufeld, 2015). It will also keep the whole community without subsetting, if choose = "all". The output could be relative abundance (outype = "Relabund") or absolute counts (outype = "counts"). If sort is TRUE, the output result will be sorted by the descending order of mean relative abundance across samples.
}
\value{
This function will return an OTU table (data frame) according to the specified arguments.
}
\references{
Lynch MDJ, Neufeld JD (2015). Ecology and exploration of the rare biosphere. Nature Reviews Microbiology 13: 217-229.
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(otuqiime)

example1 <- subOTU(otutab = otuqiime, siteInCol = TRUE, taxhead ="taxonomy",
    percent = FALSE, choose = "abundant", threshold = 1, outype = "Relabund")
dim(example1)

example2 <- subOTU(otutab = otuqiime[,-454], siteInCol = TRUE, taxhead = NULL,
    percent = FALSE, choose = "rare", threshold = 1, outype = "counts")
dim(example2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{subset }% use one of  RShowDoc('KEYWORDS')
