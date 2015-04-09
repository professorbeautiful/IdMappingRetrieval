%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationAffx.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AnnotationAffx}
\docType{class}
\alias{AnnotationAffx}


\title{The AnnotationAffx class}

\description{
 Package:   \cr
\bold{Class AnnotationAffx}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{Annotation}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{AnnotationAffx}\cr

\bold{Directly known subclasses:}\cr
\link{AnnotationNetAffx}\cr

public static class \bold{AnnotationAffx}\cr
extends \emph{\link{Annotation}}\cr



The AnnotationAffx class encapsulates the functionality allowing to retrieve data from the
Affymetrix annotation data online repository through the getIdMap() and getDataFrame() calls.
}

\usage{AnnotationAffx(cacheFolderName="Affymetrix", primaryColumn="Probe.Set.ID", secondaryColumn="SwissProt", swap=TRUE, ...)}

\arguments{
\item{cacheFolderName}{ The path to a service cashing directory for a given AnnotationAffx object.
 The path is relative to the caching subsystem root directory. Default is 'Affymetrix'}
\item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'Probe.Set.ID'.}
\item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'SwissProt'.}
\item{swap}{ Logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
retrieval during the getIdMap() call.Default is \code{\link[base:logical]{TRUE}}.}
\item{...}{ Additional parameters, see \code{\link{Annotation}}.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
}


 \bold{Methods inherited from Annotation}:\cr
getArrayType, getArrayTypes, getColumns, getCredentials, getDataFrame, getFolderName, getIdMap, getRoot, getServiceRoot, init, readDF, setCredentials, setOptions

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\author{Alex Lisovich, Roger Day}
\keyword{classes}
