%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationAffx.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setCredentials.AnnotationAffx}
\alias{setCredentials.AnnotationAffx}
\alias{AnnotationAffx.setCredentials}
\alias{setCredentials.AnnotationAffx}
\alias{setCredentials,AnnotationAffx-method}


 \title{Create and store the credentials information for Affymetrix service}

 \description{Create and store the credentials information for Affymetrix service.}

\usage{AnnotationAffx$setCredentials(verbose=FALSE, ...)}

 \arguments{
 \item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional parameters (user ID and password)}
 }

 \examples{\dontrun{
 Annotation$init();
 #set credentials for 'Affymetrix' service
 AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438");
 }}

 \seealso{For more information see \code{\link{AnnotationAffx}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
