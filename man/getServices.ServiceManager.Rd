%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getServices.ServiceManager}
\alias{getServices.ServiceManager}
\alias{ServiceManager.getServices}
\alias{getServices.ServiceManager}
\alias{getServices,ServiceManager-method}


 \title{Get list of annotation services encapsulated within the particular ServiceManager object}
 \description{Get list of annotation services encapsulated within the particular ServiceManager object.}

 \usage{\method{getServices}{ServiceManager}(this, ...)}

 \arguments{\item{...}{Not used.}}

 \value{List of \code{\link{Annotation}} derived services.}

 \examples{
 svm<-ServiceManager(ServiceManager$getDefaultServices());
 print(names(getServices(svm)));
 }

 \seealso{For more information see \code{\link{ServiceManager}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
