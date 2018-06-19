\name{typeConvert}
\alias{typeConvert}
%- Also NEED an "\alias" for EACH other topic documented here.
\title{
Transpose the data frame if there is data type conversion.
}
\description{
function to transpose the data frame if there is data type conversion.
}
\usage{
typeConvert(otutab,taxhead = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otutab}{
An OTU table, which can contain no taxonomy. The OTU table can be given in numeric counts or in relative abundance.
}
  \item{taxhead}{
Character, specify the header of taxonomy. By default the taxonomic column is NULL.
}
}
\details{
This function is to convert the numeric values into right type so that the downstream numeric calculation could be processed without type error.
}
\value{
This function returns a transposed OTU table. In the source OTU table the numeric values showing "character" or "factor" will be converted to right types.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(otu4type)
sapply(otu4type, class)
new <- typeConvert(as.data.frame(t(otu4type)), taxhead = "taxonomy")
sapply(new, class)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{convert }% use one of  RShowDoc('KEYWORDS')
