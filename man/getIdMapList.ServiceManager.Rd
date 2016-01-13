%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getIdMapList.ServiceManager}
\alias{getIdMapList.ServiceManager}
\alias{ServiceManager.getIdMapList}
\alias{getIdMapList.ServiceManager}
\alias{getIdMapList,ServiceManager-method}


 \title{Collect ID mapping data from various online query systems utilizing Annotation service functionality}
 \description{Collect ID mapping data from various online query systems utilizing Annotation service functionality. If called as static i.e. ServiceManager$getIdMapList(...), a set of default services
  will be used to collect data from.}

 \usage{\method{getIdMapList}{ServiceManager}(this, arrayType="menu", selection="menu", primaryKey="From", secondaryKey="To", force=FALSE, graphics=TRUE, verbose=FALSE, ...)}

 \arguments{
 \item{arrayType}{ The Affymertix microarray name. If 'menu', display a set of all available array types,
 allowing user to select one. If arrayType is an array name, the function performs the name correctness check.
 Default is 'menu'.}
 \item{selection}{ The character vector of service names which data are to be retrieved. If "menu" displays the available
 choices according to the names of services encapsulated within the ServiceManager object. Default is 'menu'.}
 \item{primaryKey}{ The name which will be assigned to each resulting IdMap object primary column
 at the end of a data retrieval process. Default is 'From'.}
 \item{secondaryKey}{ The name which will be assigned to each resulting IdMap object secondary column
 at the end of a data retrieval process. Default is 'To'.}
 \item{force}{ If \code{\link[base:logical]{TRUE}} forces the system to update the content of service data directory even if contained the data before.}
 \item{graphics}{ If \code{\link[base:logical]{TRUE}} and selection='menu', the attempt to will be made to use graphical capabilities of the rChoiceDialogs
 package to select a subset of services to collect data from.}
 \item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
 \item{...}{Not used.}
 }

 \value{ The list of IdMap objects each of which is a result of data retrieval for a particular service.}

 \seealso{For more information see \code{\link{ServiceManager}}.}

 \examples{
 \dontrun{
   Annotation$init();
   AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);
   #create service manage object encapsulating default services
   svm<-ServiceManager(ServiceManager$getDefaultServices());
   #retrieve the ID Map list interactively selecting array type and services
   idMapList<-getIdMapList(svm,arrayType="menu",selection="menu",verbose=TRUE);
   # use a static call to collect data using default services
   idMapList<-ServiceManager$getIdMapList(arrayType="menu",selection="menu",verbose=TRUE);
 }}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
