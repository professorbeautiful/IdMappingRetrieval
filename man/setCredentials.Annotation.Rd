%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setCredentials.Annotation}
\alias{setCredentials.Annotation}
\alias{Annotation.setCredentials}
\alias{setCredentials.Annotation}
\alias{setCredentials,Annotation-method}


 \title{Create and store the credentials information for given service}

 \description{Create and store the credentials information for given service,
 Some online query services require user to register
 on their web sites providing user ID, password etc. in order to access the data,
 Affymetrix NetAffx being an example of such service. The Annotation.setCredentials functions
 allows to store the registration information in a file within the annotation cashing subsystem
 reusing this information during the subsiquent calls to the given query system.
 }

\usage{Annotation$setCredentials(cacheFolderName, verbose=FALSE, ...)}

 \arguments{
 \item{cacheFolderName}{Caharacter string representing service name.}
 \item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Additional parameters (user ID, password etc.)}
 }

 \examples{\dontrun{
 Annotation$init();
 #set credentials for 'Affymetrix' service
 Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438");
 }}

 \seealso{For more information see \code{\link{Annotation}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
