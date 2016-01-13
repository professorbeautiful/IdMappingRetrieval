%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getServiceRoot.Annotation}
\alias{getServiceRoot.Annotation}
\alias{Annotation.getServiceRoot}
\alias{getServiceRoot.Annotation}
\alias{getServiceRoot,Annotation-method}

\title{Get a root directory for a particular annotation object}

\description{
 Retrieves a root directory for a particular service within the caching subsytem
 file structure. If the static version of call used (no service object specified), returns the path to the
 caching root directory.
}

\usage{\method{getServiceRoot}{Annotation}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{A \code{\link[base]{character}} string containing the path to the service caching directory.}

\examples{
\dontrun{
  #get the path to service object caching folder
  annObj<-AnnotationDavid("DAVID",species="Homo sapiens");
  getServiceRoot(annObj);
  #get the path to caching system root directory
  Annotation$getServiceRoot();
}}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
