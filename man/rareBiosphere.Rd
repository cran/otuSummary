\name{rareBiosphere}
\alias{rareBiosphere}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
Summarize different subgroups in the rare biosphere
}
\description{
The function discriminates different fractions of rare biosphere from the input community data, based on the ratio between the maximum and minimum abundance (for details, please see Yang et al. 2017).
}
\usage{
rareBiosphere(otutab, siteInCol = TRUE, taxhead= NULL, percent = FALSE,
    threshold = 1, cutRatio = 100, cutPERare = 5, ...)
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{otutab}{
An OTU table of microbial community, which can contain a taxonomic column (if siteInCol) or row (if site in rows). Note this function requires the otu table must be given in absolute counts, not in relative abundance.
}
  \item{siteInCol}{
Logical, by default it is "TRUE", meaning the OTU table contains samples in columns and taxa in rows. Otherwise, the otu table will be transposed.
}
  \item{taxhead}{
Character, specify the header of taxonomy. By default the taxonomic column is NULL.
}
  \item{percent}{
Logical, whether the input otu table is in relative abundance. The default is FALSE.
}
  \item{threshold}{
Numeric, the threshold specify the relative abundance cutoff upon which the rare biosphere is subset.
}
  \item{cutRatio}{
Numeric, the cutRatio parameter is the ratio between the maximum and minimum non-zero absolute abundance, which specify the threshold of conditionally rare taxa (CRT).
}
  \item{cutPERare}{
Numeric, the argument cutPERare specify the threshold of permanently rare taxa (PERare) in the community according to the ratio as mentioned above.
}
  \item{...}{
arguments to be passed to/from other methods.
}
}
\details{
    The rare biopshere constitutes different fractions of rarity. The conditionally rare taxa (CRT) are generally rare across samples but can become abundant in some samples. In contrast, the permanently rare taxa (PERare) are persistently low in abundance. Different levels of rarity may have practical implications and suggest potential roles in metabolism and ecological functions (Lynch and Neufeld, 2015; Yang et al., 2017).

    This function will filter the rare biophere from the input OTU table and then discriminate different rare fractions based on the ratio between the maximum and minimum non-zero absolute abundance, which has been as an technical alternative of skewness (please see Yang et al. 2017 for details).

    The function only works with otu table with absolute abundance, for the absolute reads enable to identify of absolute singletons and doubletons.
}
\value{
The function returns a list of 4 data frames.
%%  If it is a LIST, use
\item{summaryTable }{The element "summaryTable" returns a table containing the maximum and the minimum relative abundance, the ratio of the two abundance, the grouping of rarity, whether the taxa is singleton or doubletons, with additional column of taxonomy if it is included in the input otu table.}
\item{CRT }{Table "CRT" gives the subset of the above "summaryTable" only for the conditionally rare taxa.}
\item{PERare }{Table "PERare" shows the information of permanently rare taxa.}
\item{otherRare }{Table "otherRare" summarizes the rare taxa outside the "CRT" and "PERare".}
%% ...
}
\references{
Lynch MDJ, Neufeld JD (2015). Ecology and exploration of the rare biosphere. Nature Reviews Microbiology 13: 217-229.

Yang S, Winkel M, Wagner D, Liebner S (2017). Community structure of rare methanogenic archaea: insight from a single functional group. FEMS Microbiology Ecology: fix126.
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(otumothur)

example <- rareBiosphere(otutab = otumothur, siteInCol = TRUE, taxhead = "taxonomy",
    percent = FALSE, threshold = 1, cutRatio = 100, cutPERare = 5)
length(example)
names(example)
head(example[["summaryTable"]])
head(example[["CRT"]])

example2 <- rareBiosphere(otutab = otumothur[,-454], siteInCol = TRUE,
    taxhead = NULL, percent=FALSE, threshold = 1, cutRatio = 100, cutPERare = 5)
length(example2)
names(example2)
}

% Add one or more standard keywords, see file "KEYWORDS" in the
% R documentation directory.
\keyword{summarize }% use one of  RShowDoc("KEYWORDS")
