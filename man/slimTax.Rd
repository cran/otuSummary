\name{slimTax}
\alias{slimTax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reshape heirarchical taxonomy
}
\description{
The function will trunck the heirarchical taxonomy by "from" and "to".
}
\usage{
slimTax(x, from, to, separator =';', jump=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character, structured strings to give the heirarchical taxonomy. If x is not character, please use as.character() function to convert first.
}
  \item{from}{
numeric, valid values within 1,2,...7, which represent the tax rank of domain, phylum, class, order, family, genus and species, respectively.
}
  \item{to}{
numeric, similar to parameter "from". Please note that value of "to" must not be smaller than "from".
}
  \item{separator}{
Character, for example semicolon ";" which states the seperator of taxonomic heirarchy.
}
  \item{jump}{
Logic, Setting "jump" to TRUE means only take the tax at indices "from" and "to", excluding those in between.
}
}
\details{
This function will reformat the taxonomy by the taxonomic ranks specified by "from" and "to".
}
\value{
The function will return the reshaped taxonomy.
%%  If it is a LIST, use
%% ...
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
fillTax
}
\examples{

test = 'd__Bacteria;p__Planctomycetota;c__Planctomycetes;o__Gemmatales;f__Gemmataceae;g__;s__'
slimTax(test, from=2, to=5)

data(otuqiime)
dim(otuqiime)

result <- slimTax(x = as.character(otuqiime$taxonomy), from=2, to=5, separator =';', jump=FALSE)

dim(result)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summarize }% use one of  RShowDoc('KEYWORDS')
