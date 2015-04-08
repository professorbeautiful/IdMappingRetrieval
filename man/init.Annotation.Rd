%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{init.Annotation}
\alias{init.Annotation}
\alias{Annotation.init}
\alias{init.Annotation}
\alias{init,Annotation-method}


 \title{Init annotation subsystem}

 \description{
  Init annotation subsystem. This function should be called before the annotation functionality could be used.
  Creates the annotation file subsystem root directory if not present and initializes
  the internal reference to the root directory.
 }

\usage{Annotation$init(directory="./annotationData", verbose=TRUE, ...)}

 \arguments{
 \item{directory}{The root directory of the annotation cashing subsystem. Default is './annotationData'.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} (default) enables diagnostic messages.}
 \item{...}{additional parameters}
 }

 \value{
  Returns (invisibly) the annotation root directory.
 }

 \seealso{For more information see \code{\link{Annotation}}.}

 \examples{\dontrun{Annotation$init();}}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
