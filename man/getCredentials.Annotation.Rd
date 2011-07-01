%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCredentials.Annotation}
\alias{getCredentials.Annotation}
\alias{Annotation.getCredentials}
\alias{getCredentials.Annotation}
\alias{getCredentials,Annotation-method}


 \title{Retrieve credentials for a given service}

 \description{Retrieve credentials for a given service,
 Retrieves an object containing the credentials info (user ID, password etc.)
 allowing to access the given service programmatically
 }

 \synopsis{getCredentials.Annotation(static, serviceName, ...)}
\usage{Annotation$getCredentials(serviceName, ...)}

 \arguments{
 \item{serviceName}{\code{\link[base]{character}} string representing service name.}
 }

 \value{List containing credential info}

 \examples{
 \dontrun{
 Annotation$init();
 #set credentials for 'Affymetrix' service
 Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438");
 #get credentials
 Annotation$getCredentials("Affymetrix");
 }}

 \seealso{For more information see \code{\link{Annotation}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
