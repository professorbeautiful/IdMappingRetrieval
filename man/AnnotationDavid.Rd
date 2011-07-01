%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationDavid.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AnnotationDavid}
\docType{class}
\alias{AnnotationDavid}


\title{The AnnotationDavid class}

\description{
 Package:   \cr
\bold{Class AnnotationDavid}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{Annotation}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{AnnotationDavid}\cr

\bold{Directly known subclasses:}\cr
\link{AnnotationDavidCsv}\cr

public static class \bold{AnnotationDavid}\cr
extends \emph{\link{Annotation}}\cr



The AnnotationDavid class encapsulates the functionality allowing to retrieve data from the
DAVID online query system through the getIdMap() and getDataFrame()
calls on this AnnotationDavid object.
}

\usage{AnnotationDavid(cacheFolderName="DAVID", primaryColumn="AFFYMETRIX_3PRIME_IVT_ID", secondaryColumn="UNIPROT_ACCESSION", swap=TRUE, ...)}


\arguments{
\item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationDavid object.}
\item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'AFFYMETRIX_3PRIME_IVT_ID'.}
\item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'UNIPROT_ACCESSION'.}
\item{swap}{ A \code{\link[base]{logical}} indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
retrieval during the getIdMap() call.Default is \code{\link[base:logical]{TRUE}}.}
\item{...}{Additional parameters, see \code{\link{Annotation}}.}
}

\examples{
\dontrun{
 Annotation$init();
 #create DAVID annotation object
 annObj<-AnnotationDavid"DAVID",species="Homo sapiens");
}}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\emph{No public methods defined}.


 \bold{Methods inherited from Annotation}:\cr
getArrayType, getArrayTypes, getColumns, getCredentials, getDataFrame, getFolderName, getIdMap, getRoot, getServiceRoot, init, readDF, setCredentials, setOptions

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\author{Alex Lisovich, Roger Day}
\keyword{classes}
