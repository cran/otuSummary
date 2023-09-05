\name{calc_bc}
\alias{calc_bc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the Bray-Curtis dissimilarity
}
\description{
This function will calculate the Bray-Curtis distance matrix for community data.
}
\usage{
calc_bc(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
data frame or matrix, i.e. OTU or ASV table with sites in rows and species in columns.
}
}
\details{
This function will calculate the dissimilarity and output as lower triangular distance matrix.
}
\value{
The function returns lower triangular distrance matrix.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Sizhong Yang <yanglzu@163.com>
}
\examples{
data(otuqiime)
mat <- calc_bc(t(otuqiime[,-ncol(otuqiime)]))
mat2 <- as.matrix(mat)
dim(mat2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{diversity }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
