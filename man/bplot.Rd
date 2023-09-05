\name{bplot}
\alias{bplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate barplot with custom controls on the x axis labels
}
\description{
barplot with custom controls on the x axis labels, e.g. rotation.
}
\usage{
bplot(data, srt = 45, yoff = 0.05, dataoff = 0.025, barcol = "grey", grid = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric, vector to plot in barplot.
}
  \item{srt}{
Numeric, rotation degree of the x axis labels.
}
  \item{yoff}{
Numeric, vertical offset of x axis labels.
}
  \item{dataoff}{
Numeric, vertical offset of data labels in relation to the bar height.
}
  \item{barcol}{
Character, color of bar. By default is grey.
}
  \item{grid}{
Logical, whether show the grids in the plot.
}
  \item{...}{
arguments to be passed to/from other methods.
}
}
\examples{
data(otumothur)

summaryInfo <- otuReport(otutab = otumothur, siteInCol = TRUE, taxhead = "taxonomy",
    platform = "qiime", percent = FALSE, taxlevel = "phylum", collap = ";")

length(summaryInfo)
names(summaryInfo)
summaryInfo[[1]]

bplot(summaryInfo[["taxaFreqs"]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot }% use one of  RShowDoc("KEYWORDS")
