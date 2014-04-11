\name{InCHlib}
\alias{InCHlib}
\title{Create an InCHlib heatmap}
\usage{
InCHlib(hclustRow, hclustCol, valDf, metaDf = NA)
}
\arguments{
  \item{hclustRow}{a hclust object}

  \item{hclustCol}{a hclust object}

  \item{valDf}{a data.frame containing the data values}

  \item{metaDf}{a data.frame containing the metadata}
}
\description{
Create an InCHlib heatmap
}
\examples{
hc <- hclust(dist(USArrests), "ave")
hc.col <- hclust(dist(t(USArrests)), "ave")
inch <- InCHlib(hc, hc.col, USArrests)
library(rjson)
writeLines(toJSON(inch), "heatmap.json")
}

