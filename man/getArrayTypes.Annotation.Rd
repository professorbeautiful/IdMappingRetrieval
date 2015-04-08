%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getArrayTypes.Annotation}
\alias{getArrayTypes.Annotation}
\alias{Annotation.getArrayTypes}
\alias{getArrayTypes.Annotation}
\alias{getArrayTypes,Annotation-method}


\title{Get all available microarray chip types}

\description{Get all available microarray chip types.}

\usage{Annotation$getArrayTypes(...)}

\arguments{
 \item{...}{Not used.}
}

\value{A \code{\link[base]{character}} vector containing the names of all available microarrays.}

\examples{
\dontrun{
 Annotation$init();
 #get Affymetrix microarray array types
 arrayTypes<-Annotation$getArrayTypes();
}}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
