%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationEnsembl.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColumns1.AnnotationEnsembl}
\alias{getColumns1.AnnotationEnsembl}
\alias{AnnotationEnsembl.getColumns1}
\alias{getColumns1.AnnotationEnsembl}
\alias{getColumns1,AnnotationEnsembl-method}


\title{Retrieve the ID match pairs}

\description{
Retrieve the ID match pairs  using the algorithm corresponding to full.merge = FALSE
flag of the given AnnotationEnsemblCsv object.
}

\synopsis{getColumns1.AnnotationEnsembl(static, df, primaryColumn, secondaryColumn, verbose=FALSE, ...)}
\usage{AnnotationEnsembl$getColumns1(df, primaryColumn, secondaryColumn, verbose=FALSE, ...)}

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
