\name{getCount}
\alias{getCount}
\title{Get the number of leafs in node i}
\usage{
getCount(i, cluster)
}
\arguments{
  \item{i}{node index}

  \item{cluster}{a hclust object}
}
\description{
Get the number of leafs in node i
}
\examples{
#hc <- hclust(dist(USArrests), "ave")
#getCount(-15, hc)    # 1
#getCount(40, hc)     # 9
}

