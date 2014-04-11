\name{getParent}
\alias{getParent}
\title{Get the parent node of i}
\usage{
getParent(i, cluster)
}
\arguments{
  \item{i}{node index}

  \item{cluster}{a hclust object}
}
\description{
Get the parent node of i
}
\examples{
#hc <- hclust(dist(USArrests), "ave")
#getParent(-15, hc)   # 1
}

