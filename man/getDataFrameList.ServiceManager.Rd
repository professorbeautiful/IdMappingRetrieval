%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getDataFrameList.ServiceManager}
\alias{getDataFrameList.ServiceManager}
\alias{ServiceManager.getDataFrameList}
\alias{getDataFrameList.ServiceManager}
\alias{getDataFrameList,ServiceManager-method}


 \title{Collect raw data data from various online query systems utilizing Annotation service functionality}
 \description{Collect raw data data from various online query systems utilizing Annotation service functionality.If called as static i.e. ServiceManager$getIdMapList(...), a set of default services
 (see \code{\link[IdMappingRetrieval:getDefaultServices.ServiceManager]{getDefaultServices}})will be used to collect data from. }

 \usage{\method{getDataFrameList}{ServiceManager}(this, arrayType="menu", selection="menu", force=FALSE, graphics=TRUE, verbose=FALSE, ...)}

 \arguments{
 \item{arrayType}{ The Affymertix microarray name. If 'menu', display a set of all available array types
 allowing user to select one. If arrayType is an array name, the function performs the name correctness check.
 Default is 'menu'.}
 \item{selection}{ The character vector of service names which data are to be retrieved. If "menu" displays the available
 choices according to the names of services encapsulated within the ServiceManager object. Default is 'menu'.}
 \item{force}{ If TRUE forces the system to update the content of service data directory even if contained the data before.}
 \item{graphics}{ If \code{\link[base:logical]{TRUE}} and selection='menu', the attempt to will be made to use graphical capabilities of the rChoiceDialogs
 package to select a subset of services to collect data from.}
 \item{verbose}{ if TRUE enables diagnostic messages.}
 \item{...}{Not used.}
 }

 \value{A \code{\link[base]{list}} of data frames each of which is a result of data retrieval for a particular service.}

 \seealso{For more information see \code{\link{ServiceManager}}.}

 \examples{
 \dontrun{
   Annotation$init();
   AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);
   #create service manage object encapsulating default services
   svm<-ServiceManager(ServiceManager$getDefaultServices());
   #retrieve the data frame list interactively selecting array type and services
   dfList<-getDataFrameList(svm,arrayType="menu",selection="menu",verbose=TRUE);
   # use a static call to collect data using default services
   idMapList<-ServiceManager$getDataFrameList(arrayType="menu",selection="menu",verbose=TRUE);
 }}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
