\name{subsetByColumn}
\alias{subsetByColumn}
\title{Extract subset of rows from a data frame or a list of data frames...}
\usage{subsetByColumn(frameData, subset, column, verbose=FALSE)}
\description{Extract subset of rows from a data frame or a list of data frames
by intersecting on a particular column}
\details{This function outputs a data frame which set of rows is a product of intersecting on a particular column
of the imput data frame(s) rows with a given set of names (subset).}
\value{data frame or list of data frame which column set is a product of
intersecting of a particular column with a (partially intersecting) subset.}
\keyword{internal}
\author{Roger Day, Alex Lisovich}
\arguments{\item{frameData}{input data frame or a list of data frames.}
\item{subset}{character vector of names partially intersecting with given data frame 
column and defining the set on which merging is to be performed.}
\item{column}{The column on which the intersection is to be performed.}
\item{verbose}{if TRUE enables diagnostic messages.}}
\examples{\dontrun{
commonProteins<-subsetByRow(experimentSet,proteins,column=1);
}}
