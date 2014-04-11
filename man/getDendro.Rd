\name{getDendro}
\alias{getDendro}
\title{Get a dendrogram using the InCHlib format}
\usage{
getDendro(cluster, leafNames, values = NA)
}
\arguments{
  \item{cluster}{a hclust object}

  \item{leafNames}{a character list containing the names of
  each leaf}

  \item{values}{a data.frame containing the data values (if
  row dendrogram)}
}
\description{
Get a dendrogram using the InCHlib format
}
\examples{
#hc <- hclust(dist(USArrests), "ave")
#hc.col <- hclust(dist(t(USArrests)), "ave")
#getDendro(hc, rownames(USArrests), USArrests)
#getDendro(hc.col, colnames(USArrests))
}

