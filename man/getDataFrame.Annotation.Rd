%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getDataFrame.Annotation}
\alias{getDataFrame.Annotation}
\alias{Annotation.getDataFrame}
\alias{getDataFrame.Annotation}
\alias{getDataFrame,Annotation-method}


 \title{Get the entire data set available from a particular service in a form of a data frame}

 \description{
 Get the entire data set available from a particular service in a form of a data frame caching the
 data in a service directory allowing the fast retrieval of data next time the function is called.
 }

 \usage{\method{getDataFrame}{Annotation}(this, arrayType="menu", force=FALSE, graphics=TRUE, verbose=FALSE, ...)}

 \arguments{
  \item{arrayType}{ Character string representing the array name. If 'menu', the list of available arrays is displayed
  allowing to select the array type of interest.}
  \item{force}{ If \code{\link[base:logical]{TRUE}} forces the system to update the content of service data directory
  even if contained the data before. Default is \code{\link[base:logical]{FALSE}}.}
  \item{graphics}{ If \code{\link[base:logical]{TRUE}} (default), the attempt to will be made to use graphical capabilities of the rChoiceDialogs package to
  display selection choices (items, files etc.) in a graphical form.}
  \item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
  \item{...}{Not used.}
 }

 \value{A \code{\link[base]{data.frame}} containing entire data set available from a particular service.}

 \examples{
 \dontrun{
 Annotation$init();
 Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438",verbose);
 #create Affymetrix annotation object
 annObj=AnnotationAffx("Affymetrix");
 #retrieve the raw annotation data frame
 df<-getDataFrame(annObj,verbose=TRUE);
 }}

 \seealso{For more information see \code{\link{Annotation}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
