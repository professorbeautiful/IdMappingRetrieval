%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getIdMap.Annotation}
\alias{getIdMap.Annotation}
\alias{Annotation.getIdMap}
\alias{getIdMap.Annotation}
\alias{getIdMap,Annotation-method}


\title{Get an IdMap object using the data retrieved by a particular service represented by annotation object}
\description{Get an IdMap object using the data retrieved by a particular service represented by annotation object.}

\usage{\method{getIdMap}{Annotation}(this, arrayType="menu", primaryKey=NULL, secondaryKey=NULL, force=FALSE, graphics=TRUE, verbose=FALSE, ...)}

\arguments{
\item{arrayType}{ A \code{\link[base]{character}} string representing the array name. If 'menu', the list of available arrays is displayed
allowing to select the array type of interest.}
\item{primaryKey}{ The name which will be assigned to the IdMap object primary column
at the end of a data retrieval process.}
\item{secondaryKey}{ The name which will be assigned to the IdMap object secondary column
at the end of a data retrieval process.}
\item{force}{ If \code{\link[base:logical]{TRUE}} forces the system to update the content of service data directory even if contained the data before.
Used to update the annotation data if necessary. Default is \code{\link[base:logical]{FALSE}}.}
\item{graphics}{ If \code{\link[base:logical]{TRUE}} (default), the attempt to will be made to use graphical capabilities
of the rChoiceDialogs package to display selection choices (items, files etc.) in a graphical form.}
\item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
\item{...}{Additional parameters.}
}

\value{An ID Map object}

\examples{
\dontrun{
  Annotation$init();
  Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438",verbose);
  #create Affymetrix annotation object
  annObj=AnnotationAffx();
  #retrieve the ID Map
  idMap<-getIdMap(annObj,arrayType="HG-U133_Plus_2",primaryKey="Affy",secondaryKey="Uniprot",verbose=TRUE);
}}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
