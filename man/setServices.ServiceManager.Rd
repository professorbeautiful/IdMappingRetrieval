%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setServices.ServiceManager}
\alias{setServices.ServiceManager}
\alias{ServiceManager.setServices}
\alias{setServices.ServiceManager}
\alias{setServices,ServiceManager-method}


 \title{Set the list of services for a ServiceManager object}
 \description{Set the list of services for a ServiceManager object.}

 \usage{\method{setServices}{ServiceManager}(this, services, ...)}

 \arguments{
 \item{services}{A \code{\link[base]{list}} of services to be set within the ServiceManager object.}
 \item{...}{Not used.}
 }

 \value{List of \code{\link{Annotation}} derived services.}

 \examples{
 #create empty service manager object
 svm<-ServiceManager(ServiceManager$getDefaultServices());
 print(names(getServices(svm)))

 #redefine set of services
 setServices(svm,list(
	NetAffx_F2=AnnotationAffx("Affymetrix_3_20_10"),
	EnSembl_Q2=AnnotationEnsembl("Ensembl_3_20_10",species="Homo sapiens")
 ));
 print(names(getServices(svm)));
 }

 \seealso{For more information see \code{\link{ServiceManager}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
