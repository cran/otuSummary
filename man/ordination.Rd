\name{ordination}
\alias{ordination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quickly check the constrained or unconstrained ordination plot
}
\description{
This is a wrapped function to quickly generate a diagnostic ordination plot. It can visualize the rda and cca, either in constrained or unconstrained, as well as the NMDs.
}
\usage{
ordination(otutab, env=NULL, SiteInRow=TRUE, percent=TRUE, mySite=NULL, ordtype ="urda",
    scale = TRUE, display = "sites", choices = 1:2, biplot = FALSE,
    legPos = "bottomright", showsite = FALSE, saveplot = FALSE, ...)
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{otutab}{
A OTU table of microbial community, without a taxonomy column.
}
  \item{env}{
Optional, a environmental table, which should contains the same samples as the OTU table. If given, a constrained ordination will be processed. If env is NULL as default, the ordination will be unconstrained.
}
  \item{SiteInRow}{
Logical, if "TRUE", the OTU table contains samples in rows and taxa in columns, as this function is dependent on several vegan functions and thus follows its data format. The function will transpose the otu table if "SiteInRow" is FALSE.
}
  \item{percent}{
Logical, specify whether the input otu table are in relative abundance.
}
  \item{mySite}{
Factor, specify the sample grouping in the ordination plot, which is NULL by default.
}
  \item{ordtype}{
Character, specify the type of ordination, the valid choice are c("urda", "ucca", "unmds") for unconstrained ordination, and c("c_rda", "c_cca", "c_nmds") for constrained ones.
}
  \item{scale}{
Logical, whether scale species to unit variance (like correlations).
}
  \item{display}{
Character, to access scores for "sites" or "species".
}
  \item{choices}{
Numeric, vector of length two. Specify which axes to be shown, e.g. choices = 1:2, choices = c(1,3).
}
  \item{biplot}{
Logical, draws a biplot with species scores indicated by biplot arrows, only works in unconstrained ordination.
}
  \item{legPos}{
Character, the position of the legend in the plot. The valid choices are c("topleft", "topright", "bottomleft" and "bottomright")
}
  \item{showsite}{
Logical, whether show the site labels in the ordination.
}
  \item{saveplot}{
Logical, whether the ordination plot will be saved to the default work directory.
}
  \item{...}{
arguments, including graphical parameters of ordination object in vegan, passed to other methods.
}
}
\details{
The function wrapped several functions in 'vegan' package to quickly have a diagnostic ordination for your data. For better performance and more custom controls on plotting, please use the corresponding functions in the package 'vegan' (Oksanen et al, 2013).
}
\references{
Oksanen J, Blanchet FG, Kindt R, Legendre P, Minchin PR, O'Hara RB et al (2013). vegan: Community Ecology Package. R package version 2.0-7. http://CRAN.R-project.org/package=vegan.
}
\author{
Sizhong Yang <yanglzu@163.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The vegan manual at https://cran.r-project.org/web/packages/vegan/index.html
}
\examples{
data(varespec)
data(varechem)

ordination(otutab = varespec, env = NULL, SiteInRow = TRUE, percent = FALSE,
    mySite = NULL, ordtype = "urda", display = "sites", biplot = FALSE,
    legPos = "bottomright", showsite = FALSE, saveplot = FALSE)

ordination(otutab = varespec, env = varechem, SiteInRow = TRUE, percent = FALSE,
    mySite = NULL, ordtype = "c_rda", display = "sites", biplot = FALSE,
    legPos = "bottomright", showsite = FALSE, scale = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot }% use one of  RShowDoc('KEYWORDS')
