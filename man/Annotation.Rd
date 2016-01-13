%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Annotation}
\docType{class}
\alias{Annotation}

 \encoding{latin1}

 \title{The Annotation class}

 \description{
  Package:   \cr
\bold{Class Annotation}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\emph{\code{Annotation}}\cr

\bold{Directly known subclasses:}\cr
\link{AnnotationAffx}, \link{AnnotationEnsembl}, \link{AnnotationEnsemblCsv}, \link{AnnotationEnvision}, \link{AnnotationNetAffx}\cr

public abstract static class \bold{Annotation}\cr
extends \link[R.oo]{Object}\cr



 This is the base annotation class from which the concrete classes
 like AnnotationAffx, AnnotationEnvision etc. are derived
 }

 \usage{Annotation(cacheFolderName="", primaryColumn="From", secondaryColumn="To", swap=FALSE, species="Homo sapiens", verbose=FALSE, ...)}

 \arguments{
   \item{cacheFolderName}{ The path to a service cashing directory for a given Annotation object.
     The path is relative to the caching subsystem root directory. Default is 'Affymetrix'}
   \item{primaryColumn}{Primary column to be retrieved from a data frame obtained for a given service
     when getIdMap() on a given annotation object is called. Default is 'From'.}
   \item{secondaryColumn}{Secondary column(s) to be retrieved from a data frame obtained for a given service
     when getIdMap on a given annotation object is called. Default is 'To'.}
   \item{swap}{A \code{\link[base]{logical}} indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
     retrieval during the getIdMap() call.Default is \code{\link[base:logical]{FALSE}}.}
   \item{species}{A \code{\link[base]{character}} vector or \code{\link[base]{NA}} indicating if filtering of the results on a particular set of species
     should be performed if a given service provides the species information.
     If \code{\link[base]{NA}}, no filtering is performed. Default is "Homo sapiens".}
   \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
   \item{...}{Additional parameters}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingRetrieval:getDataFrame.Annotation]{getDataFrame}} \tab Get the entire data set available from a particular service in a form of a data frame.\cr
 \tab \code{\link[IdMappingRetrieval:getFolderName.Annotation]{getFolderName}} \tab Get caching folder name for a given Annotation object.\cr
 \tab \code{\link[IdMappingRetrieval:getIdMap.Annotation]{getIdMap}} \tab Get an IdMap object using the data retrieved by a particular service represented by annotation object.\cr
 \tab \code{\link[IdMappingRetrieval:getServiceRoot.Annotation]{getServiceRoot}} \tab Get a root directory for a particular annotation object.\cr
 \tab \code{\link[IdMappingRetrieval:setOptions.Annotation]{setOptions}} \tab Set the parameters for an annotation object.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
