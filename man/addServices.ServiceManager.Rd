%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{addServices.ServiceManager}
\alias{addServices.ServiceManager}
\alias{ServiceManager.addServices}
\alias{addServices.ServiceManager}
\alias{addServices,ServiceManager-method}


 \title{Add services to the ServiceManager object}
 \description{Add services to the ServiceManager object.}

 \usage{\method{addServices}{ServiceManager}(this, services, ...)}

 \arguments{
 \item{services}{A \code{\link[base]{list}} of services to be set within the ServiceManager object.}
 \item{...}{Not used.}
 }

 \value{ Updated list of \code{\link{Annotation}} - derived  services.}

 \examples{
 #create empty service manager object
 svm<-ServiceManager(ServiceManager$getDefaultServices());
 print(names(getServices(svm)))

 #add services
 addServices(svm,list(
	NetAffx_F2=AnnotationAffx("Affymetrix_3_20_10"),
	DAVID_Q2=AnnotationDavid("DAVID_3_20_10",species="Homo sapiens"),
	EnSembl_Q2=AnnotationEnsembl("Ensembl_3_20_10",species="Homo sapiens")
 ));
 print(names(getServices(svm)));
 }

 \seealso{For more information see \code{\link{ServiceManager}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
