% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RShipPlot.R
\name{RShipPlot}
\alias{RShipPlot}
\title{RShipPlot}
\usage{
RShipPlot(data, xvar, yvar, Nalpha = 0.1, Falpha = 0.8, palette = "Set3",
  bins = 50, lowerlimits = 0, upperlimits = 100, round.num = 3,
  pdf = FALSE, savedir)
}
\arguments{
\item{data}{a dataframe}

\item{xvar}{the x-axis variable name}

\item{yvar}{the y-axis variable name}

\item{Nalpha}{numeric variable alpha, with default 0.1}

\item{Falpha}{numeric variable alpha, with default 0.1}

\item{round.num}{exact digits}

\item{pdf}{Boolean value, whether to save as a pdf file, with default FALSE}

\item{savedir}{the pdf file save directory, like "D:/test/"}
}
\value{
RShipPlot returns a dataframe.
}
\description{
The function RShipPlot is used to group the vector.
}
\examples{
RShipPlot(data = iris,xvar = "Species", yvar = "Sepal.Width")
RShipPlot(data = iris,xvar = "Sepal.Length", yvar = "Sepal.Width", Nalpha = 0.5, bins = 10)
}
\seealso{
VarGroupText
}
