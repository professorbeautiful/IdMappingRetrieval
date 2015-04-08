%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getDefaultServices.ServiceManager}
\alias{getDefaultServices.ServiceManager}
\alias{ServiceManager.getDefaultServices}
\alias{getDefaultServices.ServiceManager}
\alias{getDefaultServices,ServiceManager-method}


 \title{Get the default services available within the annotation system}
 \description{Get the default services available within the annotation system.}

\usage{ServiceManager$getDefaultServices(...)}

 \arguments{
 \item{...}{Not used.}
 }

 \value{ A \code{\link[base]{list}} of \code{\link{Annotation}} - derived service objects containing the following items:
 \item{Affx_Q}{\code{\link{AnnotationAffx}} object retrieving data from the Affymetrix online annotation data repository}
 \item{Affx_F}{\code{\link{AnnotationNetAffx}} object retrieving the file based data from NetAffx interactive batch query system}
 \item{DAVID_Q}{\code{\link{AnnotationDavid}} object retrieving data from DAVID online query system}
 \item{DAVID_F}{\code{\link{AnnotationDavidCsv}} object retrieving the file based data from the DAVID backend annotation system}
 \item{EnSembl_Q}{\code{\link{AnnotationEnsembl}} object retrieving data from Ensembl/Biomart online query system}
 \item{EnSembl_F}{\code{\link{AnnotationEnsemblCsv}} object retrieving the file based data from Ensembl/Biomart interactive query system}
 \item{EnVision_Q}{\code{\link{AnnotationEnvision}} objectretriving data from the Envision online query system}
 }

 \examples{
 services<-ServiceManager$getDefaultServices();
 names(services);
 }

 \seealso{For more information see \code{\link{ServiceManager}}.}

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
