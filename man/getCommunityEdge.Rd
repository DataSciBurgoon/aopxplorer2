% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/aopxplorer.R
\name{getCommunityEdge}
\alias{getCommunityEdge}
\title{Returns edge attributes for member edges}
\usage{
getCommunityEdge(g, community)
}
\arguments{
\item{g}{an object of class \code{igraph} that is the AOPN of
interest}

\item{community}{the community}
}
\value{
edge.community
}
\description{
Returns edge attributes for member edges.
}
\details{
This function returns edge attributes for member edges. This was taken from
the CyREST cytoscape_util.R file.
}

