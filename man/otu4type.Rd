\name{otu4type}
\alias{otu4type}
\docType{data}
\title{
An example OTU table with samples and taxonomy in rows and otus in columns
}
\description{
A data set containing bacterial counts from the North Temperate Lakes Microbial Observatory. Due to the last row contain taxonomy, read.table function with default setting will treat the type of each column as factor.
}
\usage{data("otu4type")}
\format{
A data frame with 591 columns (OTUs) and 454 rows (453 samples plus 1 taxonomy).
}
\details{
This data show examples of "SiteInRow" if there is a taxonomy rows. When read in, the column with numeric counts will be marked as "factor". This data set could be transposed to correct type with "typeConvert" function.
}
\source{
see the entire dataset at https://github.com/cran/OTUtable/tree/master/data
}
\examples{
data(otu4type)
sapply(otu4type, class)
new <- typeConvert(otu4type)
sapply(new, class)
}
\keyword{datasets}
