\name{pbray}
\alias{pbray}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
Partition the Bray-Curtis distance dissimilarity
}
\description{
The pbray function partitions the Bray-Curtis distance matrix based on the who community and the subset of the community data.
}
\usage{
pbray(allComm, subComm, tolower = TRUE)
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{allComm}{
A otu table of microbial community which contains sample in rows and taxa in column. The otu table should not contain a taxonomic column.
}
  \item{subComm}{
A subset of the otu table, with the same samples as allComm.
}
  \item{tolower}{
Logical, by default the function returns the lower triangular matrix.
}
}
\details{
    The Bray-Curtis dissimilarity is a scaled summation of abundance differences between two communities, it is thus could be partitioned between two samples attributable to a subset of the community (Shade et al., 2014; Yang et al., 2017).
    Note, the pbray function requires the input for "allComm" and "subComm" keep consistent either in either counts or relative abundance.
    If the input for "allComm" and "subComm" are the same data, the function will return the Bray-Curtis matrix for the whole community.
}
\value{
The function returns a distance matrix by using the subset community against the whole community data.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of "comp1"}
%%  \item{comp2 }{Description of "comp2"}
%% ...
}
\references{
Shade A, Jones SE, Caporaso JG, Handelsman J, Knight R, Fierer N, and Gilbert JA. Conditionally rare taxa disproportionately contribute to temporal changes in microbial diversity. Mbio, 2014, 5(4): e01371-01314.

Yang S, Winkel M, Wagner D, and Liebner S. Community structure of rare methanogenic archaea: insight from a single functional group. FEMS Microbiology Ecology, 2017: fix126.
}
\author{
Sizhong Yang <yanglzu@163.com>
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
function contrib(), matrixConvert().
}
\examples{
data(otumothur)

subotus <- subOTU(otutab = otumothur, taxhead = "taxonomy", siteInCol = TRUE,
    percent = FALSE, choose = "rare", outype = "counts", sort = FALSE)
pRare <- pbray(allComm = t(otumothur[,-454]), subComm = t(subotus[,-454]))
class(pRare)

# convert to long format data frame

longdist <- matrixConvert(pRare, colname = c("sp1", "sp2", "bray"))
}
% Add one or more standard keywords, see file "KEYWORDS" in the
% R documentation directory.
\keyword{patition }% use one of  RShowDoc("KEYWORDS")
