\name{matrixConvert}
\alias{matrixConvert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert lower triangular distance matrix into data frame
}
\description{
This function will convert lower triangular distance matrix into a 3-column, long-format data frame.
}
\usage{
matrixConvert(triMatrix, colname = c("sp1", "sp2", "dist"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triMatrix}{
Matrix, the input matrix should be lower triangular matrix.
}
  \item{colname}{
Character, a vector of length 3 to specify the column names of the converted data frame.
}
}
\details{
This function will call the "melt" function in the reshape2 package, and convert the pairwise values in the lower triangular distance matrix into long-format data frame.
}
\value{
The function returns long format of data frame, with 3 columns. The first two columns give the pairwise names and the third column contains values in the matrix.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Sizhong Yang <yanglzu@163.com>
}
\examples{
data(varespec)  # A data frame in package vegan
mat <- vegdist(varespec, method = "bray")
mat.m <- matrixConvert(varespec, colname = c("sp1", "sp2", "bray"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{convert }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
