%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getArrayType.Annotation}
\alias{getArrayType.Annotation}
\alias{Annotation.getArrayType}
\alias{getArrayType.Annotation}
\alias{getArrayType,Annotation-method}


\title{Select the array type}

\description{Select the array type.}

\synopsis{getArrayType.Annotation(static, arrayType="menu", graphics=getOption("menu.graphics"), ...)}
\usage{Annotation$getArrayType(arrayType="menu", graphics=getOption("menu.graphics"), ...)}

\arguments{
 \item{arrayType}{ The Affymertix microarray name. If 'menu', display a set of all available array types
  allowing user to select a chip. If arrayType is an array name, the function performs the name correctness check.
  Default is 'menu'.}
 \item{graphics}{ If \code{\link[base:logical]{TRUE}} and the arrayType is 'menu', uses the graphics capabilities to display the available choices.
  Default is getOption('menu.graphics').}
}

\value{The Affymetrix chip name. If arrayType='menu' and user cancels the selection or if arrayType is an incorrect
  array name, returns \code{\link[base]{NULL}}, dysplaying the warning in the latter case.}

\examples{
\dontrun{
  Annotation$init();
  #interctively select an array type
  arrayType<-Annotation$getArrayType("menu");
}}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
