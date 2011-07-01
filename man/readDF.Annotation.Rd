%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readDF.Annotation}
\alias{readDF.Annotation}
\alias{Annotation.readDF}
\alias{readDF.Annotation}
\alias{readDF,Annotation-method}


\title{Retrieve the annotation data frame from the service data repository}

\description{
This is an internal function called from within Annotation.getDataFrame() on a particular
annotation object. The algorithm of the data frame extraction is annotation object type specific
and reflects the format and content of the data returned by a particular query system.
}

\usage{\method{readDF}{Annotation}(this, arrayType, verbose=FALSE, ...)}

\arguments{
\item{arrayType}{ The microarray chip name.}
\item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
\item{...}{ Additional arguments.}
}

\value{\code{\link[base]{data.frame}} containing annotation data for a particular microarray.}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
