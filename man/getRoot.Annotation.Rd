%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getRoot.Annotation}
\alias{getRoot.Annotation}
\alias{Annotation.getRoot}
\alias{getRoot.Annotation}
\alias{getRoot,Annotation-method}


\title{Get the path to service caching directory}
\description{Get the path to service caching directory.}

\synopsis{getRoot.Annotation(static, cacheFolderName, ...)}
\usage{Annotation$getRoot(cacheFolderName, ...)}

\arguments{
\item{cacheFolderName}{The path to service caching directory. If missing,
 returns the path to caching root directory.}
}

\value{A \code{\link[base]{character}} string representing the path to the annotation service directory.}

\examples{
\dontrun{
  Annotation$init(directory="./annotationData");
  Annotation$getRoot()
}}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
