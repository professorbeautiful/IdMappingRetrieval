%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationEnsembl.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColumns2.AnnotationEnsembl}
\alias{getColumns2.AnnotationEnsembl}
\alias{AnnotationEnsembl.getColumns2}
\alias{getColumns2.AnnotationEnsembl}
\alias{getColumns2,AnnotationEnsembl-method}


\title{Retrieve the ID match pairs}

\description{
Retrieve the ID match pairs using the algorithm corresponding to full.merge = TRUE
flag of the given AnnotationEnsemblCsv object.
}

\usage{AnnotationEnsembl$getColumns2(df, primaryColumn, secondaryColumn, verbose=FALSE, ...)}

\arguments{
\item{df}{ The data frame from which ID pairs to be retrieved.}
\item{primaryColumn}{ Primary column name.}
\item{secondaryColumn}{ Secondary column name.}
\item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
\item{...}{Not used.}
}

\value{return ID match pairs}

\seealso{For more information see \code{\link{AnnotationEnsembl}}.}
\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
