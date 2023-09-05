\name{alphaDiversity}
\alias{alphaDiversity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the alpha diversity indices
}
\description{
This function will calculate the alpha diversity indices for the total, abundant and rare biospheres.
}
\usage{
alphaDiversity(otutab, siteInCol = FALSE, taxhead = NULL, threshold = 1,
    percent = FALSE, write = FALSE, ...)
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{otutab}{
A OTU table of microbial community, which can contain a taxonomic column (if siteInCol) or row (if site in rows). The OTU table should be given in numeric (integer) counts.
}
  \item{siteInCol}{
Logical, if "TRUE", the OTU table contains samples in columns and taxa in rows. By default in this function, the siteInCol is FALSE, meaning the samples in rows.
}
  \item{taxhead}{
Character, specify the header of taxonomy if there is a taxonomic column in your data. By default this argument is NULL.
}
  \item{threshold}{
Numeric, the threshold of relative abundance upon which the rare biosphere will be subset.
}
  \item{percent}{
Logical, whether the input OTU table are given in relative abundance. FALSE means that the input OTU table is in numeric counts.
}
  \item{write}{
Logical, if TRUE, the result will be written out in a Tab separated data frame.
}
  \item{...}{
arguments to be passed to write.table().
}
}
\details{
The rare biosphere is defined by the relative abundance cutoffs (which is the "threshold" argument in this function) (Lynch and Neufeld, 2015). This update (version 0.1.2) removed the dependencies on functions "specnumber", "diversity" and "estimateR" from the R package "vegan" (Oksanen et al, 2013), and removed the "gini" function from package 'reldist' (http://www.stat.ucla.edu/~handcock/RelDist).
}
\value{
The function will return a list of length 3, including indices of observed, shannon, simpson, invsimpson, chao1, chao2, and evenness.
%%  If it is a LIST, use
\item{allBio }{The alpha diversity indices for the whole community}
\item{abundBio }{The alpha diversity indices for the abundant population}
\item{rareBio }{The alpha diversity indices for the rare biosphere}
}
\references{
Lynch MDJ, Neufeld JD (2015). Ecology and exploration of the rare biosphere. Nature Reviews Microbiology 13: 217-229.

Oksanen J, Blanchet FG, Kindt R, Legendre P, Minchin PR, O'Hara RB et al (2013). vegan: Community Ecology Package. R package version 2.0-7. http://CRAN.R-project.org/package=vegan.

}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(otumothur)

test1 <- alphaDiversity(otutab = otumothur, siteInCol = TRUE,
    taxhead = "taxonomy", threshold = 1, percent = FALSE, write = FALSE)

test2 <- alphaDiversity(otutab = otumothur[,-ncol(otumothur)], siteInCol = TRUE,
    taxhead = NULL, threshold = 1, percent = FALSE, write = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summarize }% use one of  RShowDoc('KEYWORDS')

