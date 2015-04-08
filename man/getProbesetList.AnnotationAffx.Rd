%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationAffx.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getProbesetList.AnnotationAffx}
\alias{getProbesetList.AnnotationAffx}
\alias{AnnotationAffx.getProbesetList}
\alias{getProbesetList.AnnotationAffx}
\alias{getProbesetList,AnnotationAffx-method}

\title{Get Affymetrix probeset ID list for a given array type}

\description{Get Affymetrix probeset ID list for a given array type.}

\usage{AnnotationAffx$getProbesetList(arrayType="menu", verbose=FALSE, ...)}

\arguments{
\item{arrayType}{ The Affymertix microarray name. If 'menu' (default), display a set of all available array types,
allowing user to select the array of interest.}
\item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
\item{...}{Not used.}
}

\value{A \code{\link[base]{character}} vector representing the list of Affymetrix probeset IDs for a given array type.}

\examples{
\dontrun{
  Annotation$init();
  AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);
  #retrieve probeset IDs
  AnnotationAffx$getProbesetList(arrayType="HG-U133_Plus_2",verbose=TRUE);
}}

\seealso{For more information see \code{\link{AnnotationAffx}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
