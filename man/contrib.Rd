\name{contrib}
\alias{contrib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contribution of rare/abundant biosphere to the total Bray-Curtis dissimilarity
}
\description{
function to calculate the contribution (in fraction) of the abundant or rare biosphere to the Bray-Curtis dissimilarity of the whole community.
}
\usage{
contrib(otutab, siteInCol = TRUE, taxhead = NULL, threshold = 1, percent = FALSE,
    check = "rare", write = FALSE, plot = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otutab}{
An OTU table of microbial community, which can contain taxonomy in a column or a row.
}
  \item{siteInCol}{
Logical, if "TRUE", the OTU table contains samples in columns and taxa in rows. The function will decide whether to transpose the OTU table based on this parameter.
}
  \item{taxhead}{
Character, specify the header of taxonomy, e.g. "taxonomy" if there is a taxonomy column or row. It is NULL by default.
}
  \item{threshold}{
Numeric, the threshold relative abundance cutoff upon which the rare biosphere will be subset.
}
  \item{percent}{
Logical, whether the input OTU table are in relative abundance.
}
  \item{check}{
Character, either "rare" or "abundant", telling the function which biosphere to be check.
}
  \item{write}{
Logical, if TRUE, the result will be written out as "txt" file, default is FALSE.
}
  \item{plot}{
Logical, whether the contribution result to be visualized in boxplot. By default is FALSE.
}
  \item{...}{
arguments to be passed to/from other methods.
}
}
\details{
In this function, the rare biosphere is defined by the relative abundance cutoffs (argument threshold). The Bray-Curtis distance between pairwise samples was partitioned. The Bray-Curtis measure is a scaled summation of abundance differences between two communities and can thus be partitioned for a subset population from the community (Shade et al 2014, Yang et al 2017).
}
\value{
The function will return a data frame of five columns. The first two columns specify the sample names whose Bray-Curtis distance were calculated. The third and forth columns give the distances respectively based on the whole community OTU data or the subset data. The last column gives the contribution (in fraction, not percentage) of the subset data for each pair of samples.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Shade A, Jones SE, Caporaso JG, Handelsman J, Knight R, Fierer N et al (2014). Conditionally rare taxa disproportionately contribute to temporal changes in microbial diversity. Mbio 5: e01371-01314.

Yang S, Winkel M, Wagner D, Liebner S (2017). Community structure of rare methanogenic archaea: insight from a single functional group. FEMS Microbiology Ecology: fix126.
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(otuqiime)

result <- contrib(otutab = otuqiime, siteInCol = TRUE, taxhead = "taxonomy",
    threshold = 1, percent = FALSE, check = "abund", plot = FALSE)

names(result)
head(result)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{patition }% use one of  RShowDoc("KEYWORDS")
