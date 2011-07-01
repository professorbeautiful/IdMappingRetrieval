%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColumns.Annotation}
\alias{getColumns.Annotation}
\alias{Annotation.getColumns}
\alias{getColumns.Annotation}
\alias{getColumns,Annotation-method}


\title{Extract ID match pairs from the data frame obtained from the annotation data repository}

\description{
This is an internal method called from within getIdMap() on
Annotation object. The algorithm of the ID pairs extraction is annotation object type specific
and reflects the format and content of the data frame retrieved by a prticular Annotation object.
}

\usage{\method{getColumns}{Annotation}(this, df, arrayType, verbose=FALSE, ...)}

\arguments{
 \item{df}{The \code{\link[base]{data.frame}} from which ID pairs to be retrieved.}
 \item{arrayType}{The microarray chip name.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
 \item{...}{Not used.}
}

\value{ID match pairs}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
